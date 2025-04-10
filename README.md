# Gadgetviewer documentation

## Introduction

This is a program for visualisation of 
[Gadget](https://wwwmpa.mpa-garching.mpg.de/gadget/) N-body simulation snapshots.
It can read Gadget type 1, type 2 and HDF5 snapshots and provides an 
interactive display of the particle distribution, optionally with SPH type
smoothing and colouring by any quantity that can be read from the snapshot.

There are facilities to pick out populations of particles by various
properties (e.g. temperature, density etc in SPH runs), to follow particles
between snapshots, and to make movies.

It can be used with other simulation codes which produce Gadget-like
snapshots, such as [AREPO](https://wwwmpa.mpa-garching.mpg.de/~volker/arepo/)
and [SWIFT](http://icc.dur.ac.uk/swift/), and has been used to visualize 
the [EAGLE](http://icc.dur.ac.uk/Eagle) and [Illustris](http://www.illustris-project.org/) 
simulations.

## Compilation

### Compiling a release

This package uses a configure script generated using GNU
Autoconf. If you downloaded a release .tar.gz file (which would have a 
filename along the lines of gadgetviewer-X.Y.Z.tar.gz) then the configure
script has already been generated and is included.

You can get a summary of the available options by running

./configure --help

In the unlikely event that all of the dependencies are installed in
standard locations and you don't need any special compiler flags, you
might get away with just doing

```
./configure --prefix=path
make
make install
```

where path is the directory where the program should be installed.

### Compiling from the git repository

Since the configure script and related files are machine generated
they are not included in the git repository. If you acquired the code by 
cloning the git repository or using the download source option on github 
then you'll need to ensure that you have autoconf, automake and libtool 
installed and run the autogen.sh script in the source directory. This will
generate the configure script which can be run as described above.

### Dependencies

In order to compile the program you need at least a C compiler, a
Fortran 90 compiler, and the GTK+ 3.0 GUI library. The program also has
several optional dependencies:

- HDF5  : allows reading of HDF5 snapshots
- libpng: required for writing out movie frames and screenshots
- PlPlot: to make scatterplots and histograms of particle properties
  (plplot is no longer necessary as gadgetviewer can use Cairo,
  which is a dependency of Gtk, instead)

If any of these are missing, some features will be unavailable. The
configure script generates a warning for each library which can't be found.

#### On Ubuntu linux or similar

Dependencies can be installed with
```
sudo apt install build-essential gfortran libhdf5-dev libgtk-3-dev
```

#### On CentOS/Rocky linux

Dependencies can be installed with
```
sudo dnf install gfortran hdf5-devel gtk3-devel
```

### Specifying compilers and flags

The compilers to use can be specified by setting the following
environment variables before running the configure script:

  * CC      - C compiler
  * CFLAGS  - C compiler flags
  * FC      - Fortran 90 compiler
  * FCFLAGS - Fortran compiler flags

By default the configure script will add some compiler flags which are usually
helpful. These are used in addition to anything you specify with the FCFLAGS 
and CFLAGS environment variables. If this causes problems you can configure with

--disable-optimization --disable-heap-arrays --disable-openmp

to prevent it adding extra flags.

### OpenMP

The program can make use of multiple processor cores using OpenMP if
your Fortran compiler supports it. The configure script should now
detect OpenMP if it is available.

If that doesn't work you could try adding your compiler's OpenMP flag 
to the FCFLAGS environment variable before running configure (e.g.
-fopenmp for gfortran or -openmp for ifort).

### Specifying library locations

If libraries are installed in non-standard locations (i.e. not
somewhere like /usr/lib), the following parameters can be used to tell
configure where to find the libraries:

```
    --with-hdf5=...           - specify path to HDF5
    --with-plplot=...         - specify path to PlPlot
    --with-png=...            - specify path to libpng
```

The supplied directory should contain the 'include' and 'lib'
subdirectories for the corresponding library. E.g. if libhdf5.so is
in /opt/local/hdf5/current/lib/ then you would use

```
./configure --with-hdf5=/opt/local/hdf5/current/
```

The program can be compiled without a particular library by doing
something like:

```
./configure --without-hdf5
```

If you're having problems linking a library you can use the LIBS and
LDFLAGS environment variables to pass extra flags to the linker.

Some points to note:

* To read snapshots larger than 2Gb the program needs to be compiled in
  64 bit mode, in which case all of the libraries need to be compiled
  as 64 bit too (this should no longer be a problem now that most systems
  are 64 bit).

* Libraries installed by a package manager sometimes don't include the
  header files which are necessary to compile anything that uses the
  library. It may be necessary to install a separate "dev" package with
  the headers.

* gfortran will compile this program but you need at least version
  4.2 to be able to read binary snapshots. Stream I/O is not
  implemented in 4.1.

### Example compilation

Compiling the code with the Intel compilers and installing to our own
home directory (assuming we're using the bash shell):

```
./configure CC=icc FC=ifort --prefix=$HOME/.local/
make
make install
```

To configure for debugging with gdb:
```
./configure FCFLAGS='-g -check all -fpe0 -warn -traceback -debug extended' \
            CFLAGS='-g -warn -traceback -debug extended' --prefix=$HOME/.local/ \
            --disable-optimization
```

### PlPlot configuration

Note that PlPlot is not required if Cairo is available (which it usually is).

If you're having problems compiling PlPlot then as a last resort you
could try disabling features that the gadget file viewer doesn't
need. To do this, pass the following parameters to cmake:

```
    -DENABLE_tcl=OFF
    -DENABLE_cxx=OFF
    -DENABLE_gnome2=OFF
    -DENABLE_wxwidgets=OFF
    -DENABLE_DYNDRIVERS=OFF
    -DPLD_gcw=OFF
    -DPLD_wxwidgets=OFF
    -DPLD_hp7470=OFF
    -DPLD_hp7580=OFF
    -DPLD_lj_hpgl=OFF
```

Gadgetviewer just needs the Fortran interface and the 'mem' output driver,
which is built in and can't be disabled.

### HDF5 configuration

It should now be possible to use any HDF5 1.6 or later installation.
Only the C interface is used, so it doesn't matter if the HDF5 Fortran
interface was not built or was built with a different compiler.

If HDF5 is compiled without compression support then compressed HDF5 
snapshots will be unreadable.

## Using the Gadget file viewer

### Command line flags

To get a full list of command line options, run

```
gadgetviewer --help
```

### Reading a snapshot

The name of a snapshot file to read can be specified on the command
line or a file can be selected through the File menu. If the file is
part of a multi file snapshot then the other files will be read too.
Binary (type 1 and type 2) and HDF5 snapshots can be read.

Positions, IDs and masses are always read for all particles. Other
particle properties may also be loaded depending on the file format
(see 2.8 below).

### Reading part of a snapshot

Sub-regions can be read from snapshots using the --region command line
flag. E.g.

```
gadgetviewer --region=x,y,z,r snapshot_010.hdf5
```

Here x,y,z are the coordinates of the centre of the region and r is the
radius. In case of EAGLE or SWIFT snapshots the code will use the spatial
indexing in the snapshot to efficiently extract the requested region.
Otherwise the program reads the entire snapshot and discards particles
outside the region. This will be slow for large snapshots.

It's possible to read a random sample of particles from a snapshot:

```
gadgetviewer --sample-rate=s snapshot_010.hdf5
```

where the sample rate s is in the range 0-1.

These options are also available through the "Read part of snapshot" option
in the File menu.

### Random sampling for display

Displaying all of the particles which have been read in may be too slow
for interactive use, so by default the program shows up to 2,000,000
particles. If there are more than this a random sample is shown. If you
zoom in on a particular region of the simulation you can make it draw the
sample from just this region by clicking the resample button. Alternatively
you can activate the automatic resampling option in the View menu. This
resamples the particles whenever the zoom factor changes by some amount.
Its best not to use this in conjunction with the smoothed density plot
because the smoothing lengths are recalculated whenever the particles are
resampled.

The maximum number of particles to show can be changed with the
View/View parameters option.


### Using a 3D display

To use it with a stereoscopic display which expects side by side
images, load a snapshot then select "Side by side stereo" from
the View menu and press the full screen button at the top of the
window. A slider on the toolbar allows you to adjust the eye separation
to get a comfortable 3D effect.


### Making movies

Select "Make movie" from the options menu. "Rotating movie" makes a
movie of the particle distribution rotating about the selected
point. "Evolving movie" loops over a range of snapshots and makes one
frame per snapshot, optionally re-centering the view on the selected
particles before calculating each frame. It writes out the frames as
png files so you need to use something like mencoder to encode the movie.

Evolving movies will look best if you ensure that the sampling rate is
100% before making the movie. Otherwise you get a lot of flickering
because it uses a different random sample for each frame.


### Selecting particles to follow between snapshots

"Select particles" under the Options menu allows you to highlight
particles according to property values and/or their distance from the
selected point. It uses the IDs from the snapshot file to identify
particles, so if you have a simulation where the IDs are not unique you
may get strange results. For example, any particle with the same ID as
a selected particle will be selected too. Particles stay selected when
you change snapshots, so you can use this to identify progenitors of a
particular halo, for example.

It is possible to select up to six different sets of particles
simultaneously. The drop down box at the top of the "Select particles"
window allows you to specify which set to operate on, and you can
specify a name and a colour for each one. The chosen colour is used to
highlight the selected particles in the main display and in the graph
window (see below).


### Making plots

"Make plot" in the options menu allows you to plot a histogram of the
values of a particle property or to make a scatterplot of one property
against another (e.g. temperature vs density). Particles in the current
sample are shown in red and each set of selected particles is shown in
the appropriate colour for that set. The plot will update automatically
if you choose a different sample, change snapshots or modify a
selection.

If the property plotted on the x or y axis cannot be loaded for the
current snapshot the graph window will be blank.


### Using multiple processors

Most of the code (with the exception of the octree construction on 
startup) has been parallelised with OpenMP. Multiple processor
cores should be used automatically if your compiler supports OpenMP.
You can set the number of threads to use via the OpenMP entry in the
Options menu.


### Reading extra particle properties from snapshots

#### Gadget or SWIFT HDF5 snapshots

If you have a HDF5 snapshot with extra particle properties (ages,
metallicities etc) there are two ways to make it read the extra datasets.

First, you can specify the extra datasets on the command line. E.g.
```
gadgetviewer --datasets=Temperature,Density,Metallicity ./snapshot_010.0.hdf5
```

If you'd rather make the program always read certain datasets if they're
present, then you can put the dataset names in one of the following files
depending on whether you're working with Gadget or SWIFT output:

```
.gadgetviewer_settings/gadget_hdf5_extra_properties
.gadgetviewer_settings/swift_extra_properties
```

in your home directory. Note that duplicate dataset names or names which
conflict with quantities which are always read (e.g. Mass, ID) will be
silently ignored. The default file for Gadget contains

```
[Gadget HDF5]
Extra Properties=Metallicity;StarFormationRate;Temperature;Density;InternalEnergy
```

The program will read these quantities for each type of particle in the
snapshot which has a corresponding HDF5 dataset.


#### Type 2 binary snapshots

You can specify which blocks should be read from type 2 binary
snapshots by editing the file

.gadgetviewer_settings/gadget_binary_type2_blocks

in your home directory. The default file looks like this:

```
[Gadget Type 2 Binary]
NBlocks=3;

[U]
Displayed Name=InternalEnergy;
Particle Types=0;
Data Type=REAL;

[RHO]
Displayed Name=Density;
Particle Types=0;
Data Type=REAL;

[HSML]
Displayed Name=SmoothingLength;
Particle Types=0;
Data Type=REAL;
```

For each block you need to provide

  * Four character tag for this block (in [] above)
  * Name of the particle property stored in the block ("Displayed Name")
  * Which particle types this property applies to ("Particle Types", a 
    semicolon delimited list of integers in the range 0-5)
  * Data type - this should be REAL or INTEGER. The precision of the data
    (4 or 8 byte) is detected using the record markers in the file.

Note that vector quantities (e.g. accelerations) cannot be read in this
way. There is no need to specify the POS, VEL, ID and MASS blocks
because they are always read in anyway. Entries with names or tags that
conflict with the standard set of blocks will be ignored.

#### Type 1 binary snapshots

Its not possible to read additional particle properties from type 1
snapshots because there is no way for the program to determine which
extra blocks are present.


### Reading halo finder output

Halo finder output can be read using the options under 'Read Groups' in the
File menu. Select a format, then select a file from the set generated by the
halo finder. The program will read the halo finder output corresponding to
the currently open snapshot and add one or more new particle properties
which indicate which halo each particle belongs to.

To colour particles by halo membership, select the 'colour by property' plot
type, select to colour by the appropriate dataset in the settings window, and
tick the 'use random colours for integer properties' box. The Randomize 
button can be used to reject unacceptable colour schemes. You can also use
the 'Select particles' option in the Options menu to pick out particles in
particular groups.

On changing snapshots the halo finder output for the new snapshot will be read
automatically. This depends on the files being written with a similar naming
convention to Gadget snapshots. Any instances of the snapshot number in the
path must be padded to at least three digits with leading zeros and preceded
by an underscore.

See below for the supported halo finder formats.


#### Subfind

The program can read binary output from certain (old) versions of the SubFind
halo finder. The formats which can be read are:

  * L-Gadget group_tab files - FoF groups as in the Millennium simulation
  * L-SubFind sub_tab files  - subfind groups as in the Millennium simulation
  * P-Gadget3 group_tab      - FoF groups as in the Aquarius simulations
  * P-Gadget3 subhalo_tab    - subfind groups as in the Aquarius simulations

The P-Gadget3 options wont work if the code wrote double precision outputs
or if certain preprocessor macros were enabled. To read an output select one
of the group_tab or subhalo_tab files. This will add new particle properties
FoFGroupIndex and/or SubGroupIndex. These indicate which group and subgroup
each particle belongs to, with zero indicating that the particle is in no
group. The groups are numbered in the order they appear in the halo finder output.


#### Velociraptor

The program can read HDF5 output from the VELOCIraptor halo finder
(https://github.com/pelahi/VELOCIraptor-STF). This adds the following particle
properties:

  * VR_ID_bound: which VELOCIraptor halo the particle belongs to. This
    corresponds to the ID dataset in the .catalog_groups files.
  * VR_hostHaloID_bound: which VELOCIraptor 'field' halo the particle belongs 
    to. This corresponds to the hostHaloID dataset in the .catalog_groups
    files.

Currently only the bound group membership files are read and the hostHaloID
is set equal to ID for particles which only belong to a field halo.


### Reading additional arrays from HDF5 or text files

Its possible to read in extra particle properties from ascii or HDF5
files using the "Read additional data" option in the file menu. These
files must follow the same naming convention as Gadget snapshots. Each
data array can be split across any number of files as long as there is
one element for each particle in the snapshot and the values are stored
in the same order as in the snapshot.

Once an extra property has been read the program will look for a
corresponding set of files whenever a new snapshot is loaded. This is
done by replacing any instances of the snapshot number in the
filename(s).

