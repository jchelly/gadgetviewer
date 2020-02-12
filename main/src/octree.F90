module octreemod
!
! Routines for finding particles in a specified region using a tree
! structure. Now with dynamic memory allocation for the tree nodes.
!
! MAX_DEPTH sets the maximum depth of the tree. This is so we don't try
! to allocate an infinite number of nodes in the case where lots of particles
! have exactly the same coordinates.
!
! Now updated to deal with periodic boundaries in the case of radius searches.
! Returns only periodic copies nearest the specified centre.
!

  use sort
  use data_types
  use random_shuffle
  use f90_util

  implicit none
  private 

  ! Subroutine to build the tree
  public :: build_octree
  ! Subroutines to search the tree to find particles in a particular region
  public :: range_search
  public :: radius_search
  public :: periodic_radius_search
  public :: open_leaf_node
  public :: neighbour_search
  ! Subroutine to destroy the tree
  public :: deallocate_octree
  public :: alloc_tree_workspace
  public :: free_tree_workspace

  ! Type to store a pointer to a node
  ! (can't make an array of pointers to nodes without this)
  type nodeptr_type
     private
     type(nodetype), pointer :: ptr
  end type nodeptr_type

  ! Type to store one node in the tree
  type nodetype
     private
     ! Head of linklist for this node
     integer(kind=index_kind) :: head, np
     integer                 :: nchild
     ! Pointers to child nodes
     type(nodeptr_type), dimension(0:1,0:1,0:1) :: child
     ! Pointer to parent node
     type(nodetype), pointer :: parent
     ! Pointer to next node
     type(nodetype), pointer :: next
  end type nodetype

  ! Type to store a linked list of blocks of nodes
  integer, parameter :: blocksize = 10000
  type node_block_type
     integer                                :: nnodes
     type(node_block_type), pointer         :: next
     type (nodetype), dimension(:), pointer :: node
  end type node_block_type

  ! Tree data
  type octree_type
     private
     ! Linked list
     integer(kind=index_kind), dimension(:), pointer :: list
     ! Number of particles per leaf node
     integer :: NBIN
     ! Pointer to first node
     type (nodetype), pointer :: first_node
     ! Total number of particles
     integer(kind=index_kind) :: npart
     ! Coordinate offsets
     real, dimension(3) :: posmin, posmax
     ! Box size
     real :: lbox
     ! Number of nodes
     integer :: nnodes
     ! Current node block
     type(node_block_type), pointer :: current_block
     type(node_block_type), pointer :: first_block
     ! Workspace for finding neighbours
     real,                     dimension(:,:), pointer :: rp2
     integer(kind=index_kind), dimension(:,:), pointer :: idx
     integer,                  dimension(:,:), pointer :: sortidx
  end type octree_type
  public :: octree_type, nodetype, nodeptr_type

  ! The width of one node at level 20 will be comparable to the
  ! floating point accuracy of the positions (assuming they're real*4)
  ! So there's really no point making the tree deeper than this, and having
  ! a depth limit prevents an infinite loop when lots of particles are at
  ! the same coordinates.
  integer, parameter :: MAX_DEPTH = 20

contains

  subroutine alloc_tree_workspace(tree, multithreaded)
!
! Allocate workspace needed by neighbour_search
!
    implicit none
    type (octree_type) :: tree
    integer            :: nthreads
    logical            :: multithreaded
#ifdef _OPENMP
    integer, external :: OMP_GET_MAX_THREADS
    if(multithreaded)then
       nthreads = OMP_GET_MAX_THREADS()
    else
       nthreads = 1
    endif
#else
    nthreads = 1
#endif
    allocate(tree%rp2(tree%npart, 0:nthreads-1))
    allocate(tree%idx(tree%npart, 0:nthreads-1))
    allocate(tree%sortidx(tree%npart, 0:nthreads-1))

    return
  end subroutine alloc_tree_workspace

  subroutine free_tree_workspace(tree)
!
! Free workspace needed by neighbour_search
!
    implicit none
    type (octree_type) :: tree
    deallocate(tree%rp2)
    deallocate(tree%idx)
    deallocate(tree%sortidx)

    return
  end subroutine free_tree_workspace


  function new_node(tree, first_node)
!
! Allocate a new tree node and return a pointer to it
! Also sets a pointer to the new node in the previously allocated
! node so that we get a linked list of nodes.
!
! If there is no previous node, first_node should be set to .true.
!
    implicit none
    type(nodetype), pointer :: new_node
    type (octree_type) :: tree
    integer :: i, j, k
    logical, optional :: first_node
    type (nodetype), pointer, save :: prev_node
    integer :: istat

    if(present(first_node))then
       if(first_node)nullify(prev_node)
    endif

    ! Allocate first block of nodes if necessary
    if(.not.associated(tree%first_block))then
       allocate(tree%first_block, stat=istat)
       if(istat.eq.0)allocate(tree%first_block%node(blocksize))
       if(istat.ne.0)then
          ! Return null pointer if allocation failed
          nullify(new_node)
          return
       endif
       tree%current_block => tree%first_block
       tree%current_block%nnodes = 0
       nullify(tree%current_block%next)
    endif

    ! Make sure there's at least one node left in the current
    ! block - may need to allocate a new one
    if(tree%current_block%nnodes.ge.blocksize)then
       ! Need to make a new block
       allocate(tree%current_block%next, stat=istat)
       if(istat.eq.0)allocate(tree%current_block%next%node(blocksize))
       if(istat.ne.0)then
          ! Return null pointer if allocation failed
          nullify(new_node)
          return
       endif
       nullify(tree%current_block%next%next)
       tree%current_block%next%nnodes = 0
       tree%current_block => tree%current_block%next
    endif

    ! Use the next available node in the current block,
    tree%current_block%nnodes = tree%current_block%nnodes + 1
    new_node => tree%current_block%node(tree%current_block%nnodes)

    ! Initialize new node
    new_node%head   = -1
    new_node%np     = 0
    new_node%nchild = 0
    nullify(new_node%parent)
    nullify(new_node%next)
    do i = 0, 1, 1
       do j = 0, 1, 1
          do k = 0, 1, 1
             nullify(new_node%child(i,j,k)%ptr)
          end do
       end do
    end do

    if(associated(prev_node))then
       prev_node%next => new_node
    endif
    prev_node => new_node

    return
  end function new_node


  subroutine build_octree(tree,np,pos,nb,random_order, stat)
!
! Given np particles with positions stored in pos(1:3,1:np)
! construct an octree with a maximum of nb particles in each leaf node.
!
    implicit none
    ! Parameters
    integer(kind=index_kind) :: np
    integer                 :: nb
    ! Particle data
    real(pos_kind), dimension(3,np) :: pos
    ! Return status - 0=success
    integer, optional :: stat
    ! Loop indices
    integer(kind=index_kind) :: i,j,k,inext,itemp
    integer :: ilev
    integer, dimension(3) :: ipos
    ! Pointers to nodes
    type (nodetype), pointer :: node
    integer :: levmax
    type (octree_type) :: tree
    logical, optional :: random_order
    real,    dimension(:), allocatable :: rnd
    integer(kind=index_kind), dimension(:), allocatable :: idx
    integer(kind=index_kind), dimension(:), allocatable :: partidx
    integer(kind=index_kind) :: n, ip, nmax
    integer                 :: istat

    ! Assume failure unless we get to the end
    if(present(stat))stat = -1

    ! Copy parameters into module variables
    tree%NBIN=nb
    tree%npart=np
    levmax=0

    nullify(tree%first_node)
    nullify(tree%first_block)
    nullify(tree%current_block)

    ! Allocate storage
    nullify(tree%first_node)
    allocate(tree%list(tree%npart), stat=istat)
    if(istat.ne.0)then
       if(present(stat))then
          return
       else
          call terminate("Insufficient memory to build tree")
       endif
    endif

    ! Get range of coordinates
    tree%posmax=-1.0e20
    tree%posmin=1.0e20
    do i=1,np,1
       do j=1,3,1
          if(tree%posmin(j).gt.pos(j,i))tree%posmin(j)=pos(j,i)
          if(tree%posmax(j).lt.pos(j,i))tree%posmax(j)=pos(j,i)
       end do
    end do
    
    if(np.gt.1)then
       tree%lbox=maxval(tree%posmax-tree%posmin)
    else
       ! Use arbitrary box size if there's only one particle
       ! to avoid infinite loop in radius search.
       tree%lbox = 1.0
    endif

    ! Initialise linked list of particles - all particles are in one
    ! node to start with
    do i=1,np-1,1
       tree%list(i)=i+1
    end do
    tree%first_node => new_node(tree,first_node=.true.)
    tree%nnodes = 1
    if(.not.associated(tree%first_node))then
       if(present(stat))then
          call deallocate_octree(tree)
          return
       else
          call terminate("Insufficient memory to build tree")
       endif
    endif
    tree%list(np)=-1

    ! Initial node contains all the particles
    tree%first_node%head=1
    tree%first_node%np=np
    ! and is at level 0 with integer coords (0,0,0)
    ilev=0
    ipos=0
    
    ! Recursively split volume into cubes, returning max depth reached
    levmax = split_node(ilev, ipos, tree%first_node)

    ! Find size of largest node after splitting
    nmax = 0
    node => tree%first_node
    do while(associated(node))
       nmax = max(nmax, node%np)
       node => node%next
    end do

    allocate(rnd(nmax), idx(nmax), partidx(nmax), stat=istat)
    if(istat.ne.0)then
       if(present(stat))then
          call deallocate_octree(tree)
          return
       else
          call terminate("Insufficient memory to build tree")
       endif
    endif

    ! Now scramble the ordering of particles within nodes
    if(present(random_order))then
       if(random_order)then
          ! Loop over all nodes in the tree
          node => tree%first_node
          do while(associated(node))
             if(node%np.gt.1)then
                ! Get number of particles in this node
                n = node%np
                if(n.gt.nmax)call terminate('nmax too small!')
                ! Make array to access particles in random order
                call shuffle(idx(1:n))
                ! Get indexes of particles in this node
                ip = node%head
                do i = 1, n, 1
                   partidx(i) = ip
                   ip = tree%list(ip)
                end do
                ! Rebuild the linked list in randomised order
                node%head = partidx(idx(1))
                do i = 2, n, 1
                   tree%list(partidx(idx(i-1))) = partidx(idx(i))
                end do
                tree%list(partidx(idx(n))) = -1
             endif
             node => node%next
          end do
       endif
    endif

    deallocate(rnd, idx, partidx)

    ! Success!
    if(present(stat))stat = 0

    return

  contains

    recursive function split_node(ilev, ipos, node) result(levmax)

      implicit none
      type (nodetype), pointer :: node, child_node, next_node
      integer,    dimension(3) :: ipos,ich
      real,       dimension(3) :: rdiv
      integer                  :: levmax, ilev
      integer(kind=index_kind) :: i,j,k,inext,itemp
      integer                  :: ii, jj, kk

      levmax = ilev

      ! See if this node needs to be split
      if(node%np.gt.tree%nbin.and.node%np.gt.0.and.ilev.lt.MAX_DEPTH)then
         ! This node has too many particles, so will have to split it
         do j=1,3,1
            rdiv(j)=tree%lbox/real(2**ilev)*(real(ipos(j))+0.5)
         end do
         ! Assign particles to child nodes
         i=node%head
         do while(i.ne.-1)
            inext=tree%list(i)
            do j=1,3,1
               if(pos(j,i)-tree%posmin(j).gt.rdiv(j))then
                  ich(j)=1
               else
                  ich(j)=0
               endif
            end do
            child_node => node%child(ich(1),ich(2),ich(3))%ptr
            if(.not.associated(child_node))then
               ! Need to create a new node to add the particle to
               child_node => new_node(tree)
               tree%nnodes = tree%nnodes + 1
               if(.not.associated(child_node))then
                  if(present(stat))then
                     call deallocate_octree(tree)
                     return
                  else
                     call terminate("Insufficient memory to build tree")
                  endif
               endif
               child_node%head=i
               child_node%parent => node
               child_node%np=1
               tree%list(i)=-1
               node%child(ich(1),ich(2),ich(3))%ptr => child_node
               node%nchild=node%nchild + 1
            else
               ! Particle is added to an existing node
               itemp=child_node%head
               child_node%head=i
               tree%list(i)=itemp
               child_node%np=child_node%np+1
            end if
            i=inext
         end do
         ! This node now has no particles - they've all been put into
         ! its child nodes.
         node%np=0
         node%head=-1

         ! Are there child nodes left to process?
         ! Go to the first one that still needs to be done.
         nullify(next_node)
         if(node%nchild.gt.0)then
            levmax = ilev + 1
            do ii=0,1,1
               do jj=0,1,1
                  do kk=0,1,1
                     child_node => node%child(ii,jj,kk)%ptr
                     if(associated(child_node))then
                        if(child_node%np.gt.tree%nbin.and. &
                             ilev.lt.MAX_DEPTH-1)then
                           levmax = max(levmax, split_node(ilev+1, &
                                ipos*2+(/ii,jj,kk/), child_node))
                        end if
                     end if
                  end do
               end do
            end do
         end if
      else
         ! No child nodes
         levmax = ilev
      endif

      return
    end function split_node

  end subroutine build_octree
!
! ------------------------------------------------------------------------
!
  subroutine range_search(tree,pos,rangemin,rangemax,nout,idx)
!
! Locate all particles with coordinates in the range specified by
! rangemin(1:3) and rangemax(1:3) and return their array indices in idx.
! The number of particles found is stored in nout.
!
    implicit none
    ! Input parameters
    type (octree_type),                          intent(in) :: tree
    real(pos_kind), dimension(1:3,1:tree%npart), intent(in) :: pos
    real,           dimension(3),                intent(in) :: rangemin
    real,           dimension(3),                intent(in) :: rangemax
    ! Output
    integer(kind=index_kind),               intent(out) :: nout
    integer(kind=index_kind), dimension(:), intent(out) :: idx
    ! Loop indices
    integer :: ilev,i,j,k
    integer, dimension(3) :: ipos,jpos,ich
    integer(kind=index_kind) :: ipart
    type (nodetype), pointer :: node, child_node

    if(.not.associated(tree%first_node)) &
         call terminate("range_search(): Tree has not been built yet!")
    
    nout=0
    node => tree%first_node
    ich=0
    ipos(1:3)=0
    ilev=0
    do while(associated(node))
       ! See if there's a child node to visit
       nullify(child_node)
       if(node%nchild.gt.0)then
          do while(ich(1).ne.-1.and.(.not.associated(child_node)))
             child_node => node%child(ich(1),ich(2),ich(3))%ptr
             if(.not.associated(child_node))then
                call nextchild(ich)
             else
                ! Does this child node overlap the region of interest?
                jpos=ipos*2+ich
                do j=1,3,1
                   if((tree%lbox/real(2**(ilev+1)))*jpos(j).gt. &
                        rangemax(j)-tree%posmin(j)) nullify(child_node)
                   if((tree%lbox/real(2**(ilev+1)))*(jpos(j)+1).lt. &
                        rangemin(j)-tree%posmin(j)) nullify(child_node)
                end do
                if(.not.associated(child_node))call nextchild(ich)
             end if
          end do
       end if
       
       if(.not.associated(child_node))then
          if(node%nchild.eq.0)then
             ! This is a 'leaf' node in the requested range -
             ! record its particles in the output array
             ipart=node%head
             do while(ipart.ne.-1)
                if  (pos(1,ipart).lt.rangemax(1).and. &
                     pos(1,ipart).gt.rangemin(1).and. &
                     pos(2,ipart).lt.rangemax(2).and. &
                     pos(2,ipart).gt.rangemin(2).and. &
                     pos(3,ipart).lt.rangemax(3).and. &
                     pos(3,ipart).gt.rangemin(3))then
                   nout=nout+1
                   idx(nout)=ipart
                end if
                ipart=tree%list(ipart)
             end do
          end if
          ! Node has no children or we've visited all the child nodes
          ! so go back up a level
          ilev=ilev-1
          ipos=int(ipos/2)
          if(associated(node%parent))then
             ! Figure out which child this is of the parent node
             do i=0,1,1
                do j=0,1,1
                   do k=0,1,1
                      if(associated(node,node%parent%child(i,j,k)%ptr))then
                         ich(1)=i
                         ich(2)=j
                         ich(3)=k
                         call nextchild(ich)
                      end if
                   end do
                end do
             end do
          end if
          node => node%parent
       else
          ! Go to child node
          node => child_node
          ilev=ilev+1
          ipos=ipos*2+ich
          ich=0 
       end if
    end do

  end subroutine range_search
!
! --------------------------------------------------------------------
!

  subroutine radius_search(tree,pos,centre,rmin,rmax,nout,idx,fsample)
!
! Find particles between radii rmin and rmax of coordinates in centre.
! Return indices in idx and number of particles found in nout.
!
    implicit none
    ! Input parameters
    type (octree_type),                              intent(in) :: tree
    real(pos_kind),     dimension(1:3,1:tree%npart), intent(in) :: pos
    real(pos_kind),     dimension(3),                intent(in) :: centre
    real(pos_kind),                                  intent(in) :: rmin, rmax
    real(pos_kind),                        optional, intent(in) :: fsample
    ! Output
    integer(kind=index_kind),                         intent(out) :: nout
    integer(kind=index_kind), dimension(:), optional, intent(out) :: idx    
    ! Loop indices etc
    integer :: ilev,i,j,k
    type (nodetype), pointer :: node, child_node
    integer, dimension(3) :: ipos,jpos,ich
    integer(kind=index_kind) :: ipart
    real :: rcell, drmax,rp2
    real :: srate
    integer :: ns
    real :: p, r

    ! Get sampling rate
    srate = 1.0
    if(present(fsample))srate=fsample

    ! Check tree has been built
    if(.not.associated(tree%first_node)) &
         call terminate('radius_search(): Tree has not been built yet!')

    ! Start from the root node
    nout=0
    node => tree%first_node
    ich=0
    ipos(1:3)=0
    ilev=0

    ! Find size and distance to top node
    rcell=0.0
    do j=1,3,1
       rcell=rcell+   &
            (((tree%lbox/real(2**(ilev)))*(ipos(j)+0.5))- &
            (centre(j)-tree%posmin(j)))**2
    end do
    rcell=sqrt(rcell)
    drmax=0.87*(tree%lbox/real(2**(ilev)))

    ! Walk the tree
    do while(associated(node))
       ! See if there's a child node to visit
       nullify(child_node)
       if(node%nchild.gt.0)then
          do while(ich(1).ne.-1.and.(.not.associated(child_node)))
             child_node => node%child(ich(1),ich(2),ich(3))%ptr
             if(.not.associated(child_node))then
                call nextchild(ich)
             else
                ! Could this child node overlap the region of interest?
                jpos=ipos*2+ich
                rcell=0.0
                do j=1,3,1
                   rcell=rcell+   &
                        (((tree%lbox/real(2**(ilev+1)))*(jpos(j)+0.5))- &
                        (centre(j)-tree%posmin(j)))**2
                end do
                rcell=sqrt(rcell)
                ! Find half length of cell diagonal
                ! (0.87 = sqrt(3)/2 rounded up)
                drmax=0.87*(tree%lbox/real(2**(ilev+1)))
                if(rcell+drmax.lt.rmin.or.rcell-drmax.gt.rmax) &
                     nullify(child_node)
                if(.not.associated(child_node))call nextchild(ich)
             end if
          end do
       end if

       if(.not.associated(child_node))then
          if(node%nchild.eq.0)then
             ! This is a 'leaf' node in the requested range.
             ! record its particles in the output array
             ipart=node%head

             ! Decide how many particles we're going to take
             ns = min(int(node%np*srate), node%np)
             
             ! np*srate will not usually be an integer - take an extra
             ! particle with a probability determined by the fractional
             ! part of np*srate. This is vital when np*srate < 1.0!
             ! (a better way to do this would be to draw ns from a
             ! binomial distribution, but this way is much quicker)
             p = real(dble(node%np*srate)-dble(ns))
             if(p.gt.0.and.ns.lt.node%np)then
                call random_number(r)
                if(r.lt.p)ns = ns + 1
             endif

             if(rcell+drmax.lt.rmax.and. &
                  (rcell-drmax.gt.rmin.or.rmin.eq.0.0))then
                ! If the node is entirely within the shell we can save a bit
                ! of time here by not checking individual particle positions
                if(present(idx))then
                   do i = 1, ns, 1
                      idx(nout+i)=ipart
                      ipart=tree%list(ipart)
                   end do 
                endif
                nout = nout + ns
             else
                ! Otherwise we need to examine the coordinates one particle
                ! at a time
                if(rmin.gt.0.0)then
                   ! If rmin>0 we need to check that each particle is outside
                   ! the inner surface of the shell as well as inside rmax
                   do i = 1, ns, 1
                      rp2=sum((pos(1:3,ipart)-centre(1:3))**2)
                      if(rp2.ge.rmin*rmin.and.rp2.lt.rmax*rmax)then
                         nout=nout+1
                         if(present(idx))idx(nout)=ipart
                      end if
                      ipart=tree%list(ipart)
                   end do
                else
                   ! In the (common) case where rmin=0.0 we can simplify
                   ! the if statement in the loop over particles. This loop
                   ! is nested pretty deep, so every little helps...
                   do i = 1, ns, 1
                      rp2=sum((pos(1:3,ipart)-centre(1:3))**2)
                      if(rp2.lt.rmax*rmax)then
                         nout=nout+1
                         if(present(idx))idx(nout)=ipart
                      end if
                      !write(0,*)"F"
                      if(ipart.ge.1.and.ipart.le.size(tree%list))then
                         ipart=tree%list(ipart)
                      else
                         call terminate('ipart out of range')
                      endif
                      !write(0,*)"G"
                   end do
                endif
             endif
          end if
          ! Node has no children or we've visited all the child nodes
          ! so go back up a level
          ilev=ilev-1
          ipos=int(ipos/2)
          if(associated(node%parent))then
             ! Figure out which child this is of the parent node
             do i=0,1,1
                do j=0,1,1
                   do k=0,1,1
                      if(associated(node%parent%child(i,j,k)%ptr,node))then
                         ich(1)=i
                         ich(2)=j
                         ich(3)=k
                         call nextchild(ich)
                      end if
                   end do
                end do
             end do
          end if
          node => node%parent
       else
          ! Go to child node
          node => child_node
          ilev=ilev+1
          ipos=ipos*2+ich
          ich=0 
       end if
    end do

  end subroutine radius_search

!
! -------------------------------------------------------------------------
!

  subroutine periodic_radius_search(tree,pos,boxsize,centre,rmin,rmax,nout,idx,fsample)
!
! Find particles between radii rmin and rmax of coordinates in centre.
! Return indices in idx and number of particles found in nout.
!
! This version assumes a periodic box with coordinates in the range (0,boxsize).
! Note that only the periodic copy of each particle nearest centre is ever returned,
! even if the search radius is large enough to encompass multiple periodic copies.
!
    implicit none
    ! Input parameters
    type (octree_type),                              intent(in) :: tree
    real(pos_kind),     dimension(1:3,1:tree%npart), intent(in) :: pos
    real(pos_kind),                                  intent(in) :: boxsize
    real(pos_kind),     dimension(3),                intent(in) :: centre
    real(pos_kind),                                  intent(in) :: rmin, rmax
    real(pos_kind),                        optional, intent(in) :: fsample
    ! Output
    integer(kind=index_kind),                         intent(out) :: nout
    integer(kind=index_kind), dimension(:), optional, intent(out) :: idx    
    ! Wrapped centre
    real(kind=pos_kind), dimension(3) :: wrap_centre
    ! Cell centre coordinates
    real(kind=pos_kind), dimension(3) :: cell_pos
    ! Loop indices etc
    integer :: ilev,i,j,k
    type (nodetype), pointer :: node, child_node
    integer, dimension(3) :: ipos,jpos,ich
    integer(kind=index_kind) :: ipart
    real :: rcell, drmax,rp2
    real :: srate
    integer :: ns
    real :: p, r

    ! Get sampling rate
    srate = 1.0
    if(present(fsample))srate=fsample

    ! Check tree has been built
    if(.not.associated(tree%first_node)) &
         call terminate('periodic_radius_search(): Tree has not been built yet!')

    ! Ensure requested centre is in the box
    wrap_centre = centre
    do j = 1, 3, 1
       do while(wrap_centre(j).ge.boxsize)
          wrap_centre(j) = wrap_centre(j) - boxsize
       end do
       do while(wrap_centre(j).lt.0.0)
          wrap_centre(j) = wrap_centre(j) + boxsize
       end do
    end do

    nout=0
    node => tree%first_node
    ich=0
    ipos(1:3)=0
    ilev=0
    drmax=0.87*(tree%lbox/real(2**(ilev+1)))
    cell_pos = (0.5*tree%lbox) + tree%posmin 
    rcell = sqrt(sum((cell_pos-wrap_centre)**2))
    do while(associated(node))
       ! See if there's a child node to visit
       nullify(child_node)
       if(node%nchild.gt.0)then
          do while(ich(1).ne.-1.and.(.not.associated(child_node)))
             child_node => node%child(ich(1),ich(2),ich(3))%ptr
             if(.not.associated(child_node))then
                call nextchild(ich)
             else
                ! Calculate center coordinates for this child node
                jpos=ipos*2+ich
                do j = 1, 3, 1
                   cell_pos(j) = (tree%lbox/real(2**(ilev+1)))*(jpos(j)+0.5) + tree%posmin(j)
                end do
                ! Wrap cell coordinates to periodic copy nearest the requested centre
                do j = 1, 3, 1
                   if(cell_pos(j)-wrap_centre(j).gt.0.5*boxsize)then
                      cell_pos(j) = cell_pos(j) - boxsize
                   endif
                   if(cell_pos(j)-wrap_centre(j).lt.-0.5*boxsize)then
                      cell_pos(j) = cell_pos(j) + boxsize
                   endif
                end do
                ! Get distance from requested centre to centre of cell
                rcell = sqrt(sum((cell_pos-wrap_centre)**2))
                ! Get half length of cell diagonal (0.87 = sqrt(3)/2 rounded up)
                drmax=0.87*(tree%lbox/real(2**(ilev+1)))
                ! Check if we need to open this cell
                if(rcell+drmax.lt.rmin.or.rcell-drmax.gt.rmax) &
                     nullify(child_node)
                if(.not.associated(child_node))call nextchild(ich)
             end if
          end do
       end if

       if(.not.associated(child_node))then
          if(node%nchild.eq.0)then
             ! This is a 'leaf' node in the requested range -
             ! record its particles in the output array
             ipart=node%head

             ! Decide how many particles we're going to take
             ns = min(int(node%np*srate), node%np)
             
             ! np*srate will not usually be an integer - take an extra
             ! particle with a probability determined by the fractional
             ! part of np*srate. This is vital when np*srate < 1.0!
             ! (a better way to do this would be to draw ns from a
             ! binomial distribution, but this way is much quicker)
             p = real(dble(node%np*srate)-dble(ns))
             if(p.gt.0.and.ns.lt.node%np)then
                call random_number(r)
                if(r.lt.p)ns = ns + 1
             endif

             if(rcell+drmax.lt.rmax.and. &
                  (rcell-drmax.gt.rmin.or.rmin.eq.0.0))then
                ! If the node is entirely within the shell we can save a bit
                ! of time here by not checking individual particle positions
                if(present(idx))then
                   do i = 1, ns, 1
                      idx(nout+i)=ipart
                      ipart=tree%list(ipart)
                   end do 
                endif
                nout = nout + ns
             else
                ! Otherwise we need to examine the coordinates one particle
                ! at a time
                if(rmin.gt.0.0)then
                   ! If rmin>0 we need to check that each particle is outside
                   ! the inner surface of the shell as well as inside rmax
                   do i = 1, ns, 1
                      rp2 = mindist2(boxsize, pos(1:3,ipart), wrap_centre(1:3))
                      if(rp2.ge.rmin*rmin.and.rp2.lt.rmax*rmax)then
                         nout=nout+1
                         if(present(idx))idx(nout)=ipart
                      end if
                      ipart=tree%list(ipart)
                   end do
                else
                   ! In the (common) case where rmin=0.0 we can simplify
                   ! the if statement in the loop over particles.
                   do i = 1, ns, 1
                      rp2 = mindist2(boxsize, pos(1:3,ipart), wrap_centre(1:3))
                      if(rp2.lt.rmax*rmax)then
                         nout=nout+1
                         if(present(idx))idx(nout)=ipart
                      end if
                      !write(0,*)"F"
                      if(ipart.ge.1.and.ipart.le.size(tree%list))then
                         ipart=tree%list(ipart)
                      else
                         call terminate('ipart out of range')
                      endif
                      !write(0,*)"G"
                   end do
                endif
             endif
          end if
          ! Node has no children or we've visited all the child nodes
          ! so go back up a level
          ilev=ilev-1
          ipos=int(ipos/2)
          if(associated(node%parent))then
             ! Figure out which child this is of the parent node
             do i=0,1,1
                do j=0,1,1
                   do k=0,1,1
                      if(associated(node%parent%child(i,j,k)%ptr,node))then
                         ich(1)=i
                         ich(2)=j
                         ich(3)=k
                         call nextchild(ich)
                      end if
                   end do
                end do
             end do
          end if
          node => node%parent
       else
          ! Go to child node
          node => child_node
          ilev=ilev+1
          ipos=ipos*2+ich
          ich=0 
       end if
    end do

  contains

    real(kind=pos_kind) function mindist2(boxsize, pos1, pos2) result(r2)
!
! Return minimum distance (squared) between two points taking periodic
! boundary into account
!
      implicit none
      ! Input parameters
      real(kind=pos_kind), dimension(3), intent(in) :: pos1, pos2
      real(kind=pos_kind),               intent(in) :: boxsize
      ! Internal
      real(kind=pos_kind), dimension(3) :: dr
      integer :: i

      dr = abs(pos2 - pos1)
      do i = 1, 3, 1
         if(dr(i).gt.0.5*boxsize)dr(i) = boxsize - dr(i)
      end do
      r2 = sum(dr**2)

      return
    end function mindist2

  end subroutine periodic_radius_search

!
! --------------------------------------------------------------------
!

  subroutine nextchild(ich)
!
! Advance to next child node
!
    implicit none
    integer, dimension(3) :: ich
    
    ich(1)=ich(1)+1
    if(ich(1).eq.2)then
       ich(1)=0
       ich(2)=ich(2)+1
    end if
    if(ich(2).eq.2)then
       ich(3)=ich(3)+1
       ich(2)=0
    end if
    if(ich(3).eq.2)ich=-1

  end subroutine nextchild
!
! -------------------------------------------------------------------
!
  subroutine deallocate_octree(tree)
!
! Deallocate the tree nodes and particle linklist
!
    implicit none
    type (octree_type) :: tree
    type (node_block_type), pointer :: block, next_block

    block => tree%first_block
    do while(associated(block))
       next_block => block%next
       deallocate(block%node)
       deallocate(block)
       block => next_block
    end do

    if(associated(tree%list))deallocate(tree%list)
    nullify(tree%list)
    nullify(tree%first_node)

    return
  end subroutine deallocate_octree

!
! ------------------------------------------------------------------
!

  subroutine open_leaf_node(tree,centre,nout,idx,rnodemin,rnodemax,rmin)
!
!   Find the leaf node that contains position 'centre' and return indices
!   of particles it contains. Also returns the spatial extent of the node -
!   rnodemin(1:3) contains the minimum x,y,z coordinates for a particle to be 
!   in this node and rnodemax(1:3) contains the maximum coordinates.
!
    implicit none
    ! Parameters - range to search
    real(pos_kind), dimension(3)   :: centre,rnodemin,rnodemax
    real(pos_kind) :: rmin
    ! Array to return with indices of particles in range
    integer(kind=index_kind) :: nout
    integer(kind=index_kind), dimension(:) :: idx
    ! Loop indices
    integer :: ilev,i,j,k
    type (nodetype), pointer :: node, child_node
    integer, dimension(3) :: ipos,jpos,ich
    integer(kind=index_kind) :: ipart
    type (octree_type) :: tree
    integer :: ilevmax

    if(.not.associated(tree%first_node)) &
         call terminate('open_leaf_node(): Tree has not been built yet!')
    
    nout=0
    node => tree%first_node
    ich=0
    ipos(1:3)=0
    ilev=0
    ilevmax = 0
    ! Exit if nout>0 as there can only be one leaf node containing the point
    ! 'centre' 
    do while(associated(node).and.nout.eq.0)
       ! See if there's a child node to visit
       nullify(child_node)
       if(node%nchild.gt.0)then
          do while(ich(1).ne.-1.and.(.not.associated(child_node)))
             child_node => node%child(ich(1),ich(2),ich(3))%ptr
             if(.not.associated(child_node))then
                call nextchild(ich)
             else
                ! Does this child node contain the point of interest?
                jpos=ipos*2+ich
                do j=1,3,1
                   if((tree%lbox/real(2**(ilev+1)))*jpos(j).gt. &
                        centre(j)-tree%posmin(j)) nullify(child_node)
                   if((tree%lbox/real(2**(ilev+1)))*(jpos(j)+1).lt. &
                        centre(j)-tree%posmin(j)) nullify(child_node)
                end do
                if(.not.associated(child_node))call nextchild(ich)
             end if
          end do
       end if
       
       ! Record maximum level reached
       ilevmax = max(ilev, ilevmax)

       if(.not.associated(child_node))then
          if(node%nchild.eq.0)then
             ! This is a 'leaf' node which contains the point 'centre'
             ! record its particles in the output array
             ipart=node%head
             do while(ipart.ne.-1)
                nout=nout+1
                idx(nout)=ipart
                ipart=tree%list(ipart)
             end do
             ! Record max/min coordinates of this node
             do j=1,3,1
                rnodemin(j)=((tree%lbox/real(2**ilev))*ipos(j))+tree%posmin(j)
                rnodemax(j)=((tree%lbox/real(2**ilev))*(ipos(j)+1))+tree%posmin(j)
             end do
          end if
          ! Node has no children or we've visited all the child nodes
          ! so go back up a level
          ilev=ilev-1
          ipos=int(ipos/2)
          if(associated(node%parent))then
             ! Figure out which child this is of the parent node
             do i=0,1,1
                do j=0,1,1
                   do k=0,1,1
                      if(associated(node%parent%child(i,j,k)%ptr,node))then
                         ich(1)=i
                         ich(2)=j
                         ich(3)=k
                         call nextchild(ich)
                      end if
                   end do
                end do
             end do
          end if
          node => node%parent
       else
          ! Go to child node
          node => child_node
          ilev=ilev+1
          ipos=ipos*2+ich
          ich=0 
       end if
    end do

    ! Return size of smallest node visited
    rmin = tree%lbox / (2**ilevmax)

  end subroutine open_leaf_node

!
! --------------------------------------------------------------------
!

  subroutine neighbour_search(tree,pos,centre,nngbfind,ngbidx)
    !
    ! Find the nngbfind nearest particles to position 'centre' and return
    ! their indices in the array idx.
    !
    ! Looks at the node containing 'centre' to get initial search radius
    ! then carries out radius searches of increasing size if necessary.
    !
    use sort
    implicit none

    ! Parameters
    real(pos_kind), dimension(:,:), intent(IN) :: pos
    real(pos_kind), dimension(3), intent(IN) :: centre
    integer(kind=index_kind), intent(IN) :: nngbfind
    integer(kind=index_kind), dimension(:), intent(OUT) :: ngbidx
    ! Factor by which to increase search radius if too few particles found
    real, parameter :: EXP_FAC = 1.25
    ! Internal variables
    integer(kind=index_kind) :: nout, nngb
    real(pos_kind) :: rcomplete
    real(pos_kind), dimension(3) :: rnodemin, rnodemax
    integer :: i, j
    real(pos_kind) :: rsearch, rmin
    type (octree_type) :: tree
    ! Thread index
    integer :: myid
#ifdef _OPENMP
    integer, external :: OMP_GET_THREAD_NUM
       myid = OMP_GET_THREAD_NUM()
#else
       myid = 0
#endif

    if(.not.associated(tree%first_node)) &
         call terminate('open_leaf_node(): Tree has not been built yet!')

    call open_leaf_node(tree,centre,nout,tree%idx(:,myid),rnodemin,rnodemax,rmin)
    if(nout.gt.0)then
       ! Find radius within which we have all the neighbour particles
       ! (ie shortest distance to edge of the leaf node)
       rcomplete=1.0e20
       do j=1,3,1
          rcomplete=min(rcomplete,rnodemax(j)-centre(j))
          rcomplete=min(rcomplete,centre(j)-rnodemin(j))   
       end do
       ! Have we found enough neighbours already?
       nngb=0
       do i=1,nout,1
          if(sum((pos(1:3,tree%idx(i,myid))-centre(1:3))**2).lt.rcomplete**2)then
             nngb=nngb+1
             tree%idx(nngb,myid)=tree%idx(i,myid)
          end if
       end do
       nout=nngb
       ! If not, will have to increase the search radius until we have
       ! enough particles. Set initial radius to the size of the node
       ! and multiply it by 1.25 each time a search returns too few particles.
       rsearch=abs(rnodemax(1)-rnodemin(1))
    else
       ! Point is not in a leaf node. Set search radius to size of smallest
       ! node visited during tree walk.
       rsearch = rmin
       nout = 0
    endif
    do while(nout.lt.nngbfind)
       call radius_search(tree,pos,centre,0.0_pos_kind,rsearch,nout,tree%idx(:,myid))
       if(nout.lt.nngbfind)rsearch=rsearch*EXP_FAC
    end do
    ! Now sort by radius and pick the first nngbfind
    tree%rp2(1:nout,myid)=(pos(1,tree%idx(1:nout,myid))-centre(1))**2   &
         +(pos(2,tree%idx(1:nout,myid))-centre(2))**2         &
         +(pos(3,tree%idx(1:nout,myid))-centre(3))**2
    call sort_index(tree%rp2(1:nout,myid),tree%sortidx(1:nout,myid))

    ! Return the nngbfind closest particles
    ngbidx(1:nngbfind)=tree%idx(tree%sortidx(1:nngbfind,myid), myid)

    return
  end subroutine neighbour_search


end module octreemod
