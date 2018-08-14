# Gadgetviewer documentation

## Compilation

This package uses a configure script generated using GNU
Autoconf. You can get a summary of the available options by running

./configure --help

In the unlikely event that all of the dependencies are installed in
standard locations and you don't need any special compiler flags, you
might get away with just doing

```
./configure --prefix=<path>
make
make install
```

where <path> is the directory where the program should be installed.

### Dependencies

In order to compile the program you need at least a C compiler, a
Fortran 90 compiler, and the GTK+ 2.0 GUI library. The program also has
several optional dependencies:

- HDF5  : allows reading of HDF5 snapshots
- libpng: required for writing out movie frames and screenshots
- PlPlot: to make scatterplots and histograms of particle properties
  (plplot is no longer necessary as the gadgetviewer can use Cairo,
  which is a dependency of Gtk, instead)

If any of these are missing, some features will be unavailable. The
configure script generates a warning for each library which can't be found.

### Specifying compilers and flags

The compilers to use can be specified by setting the following
environment variables before running the configure script:

  * CC      - C compiler
  * CFLAGS  - C compiler flags
  * FC      - Fortran 90 compiler
  * FCFLAGS - Fortran compiler flags

### OpenMP

The program can make use of multiple processor cores using OpenMP if
your Fortran compiler supports it. To enable OpenMP add your compiler's
OpenMP flag to the FCFLAGS environment variable before running configure
(e.g. -fopenmp for gfortran or -qopenmp for ifort).

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
  as 64 bit too.

* Libraries installed by a package manager sometimes don't include the
  header files which are necessary to compile anything that uses the
  library. It may be necessary to install a separate "dev" package with
  the headers.

* gfortran will compile this program but you need at least version
  4.2 to be able to read binary snapshots. Stream I/O is not
  implemented in 4.1.

### Example compilation

Compiling the code with the Intel compilers and OpenMP (assuming we're 
using the csh shell):

```
setenv CC icc
setenv CFLAGS  "-O3 -xSSE4.2 -fopenmp -shared-intel -no-prec-div -fp-model fast=2"
setenv FC ifort
setenv FCFLAGS "-O3 -xSSE4.2 -fopenmp -heap-arrays -shared-intel -no-prec-div -fp-model fast=2"
./configure --prefix=/usr/local/
make
make install
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

The gadget file viewer just needs the Fortran interface and the 'mem'
output driver, which is built in and can't be disabled.

### HDF5 configuration

It should now be possible to use any HDF5 1.6 or later installation.
Only the C interface is used, so it doesn't matter if the HDF5 Fortran
interface was not built or was built with a different compiler.

If HDF5 is compiled without compression support then compressed HDF5 
snapshots will be unreadable.

## Using the Gadget file viewer

### Reading a snapshot

The name of a snapshot file to read can be specified on the command
line or a file can be selected through the File menu. If the file is
part of a multi file snapshot then the other files will be read too.
Binary (type 1 and type 2) and HDF5 snapshots can be read.

Positions, IDs and masses are always read for all particles. Other
particle properties may also be loaded depending on the file format
(see 2.8 below).


### Random sampling

By default the program shows up to 500,000 particles. If there are more
than this a random sample is shown. If you zoom in on a particular
region of the simulation you can make it draw the sample from just this
region by clicking the resample button. Alternatively you can activate
the automatic resampling option in the View menu. This resamples the
particles whenever the zoom factor changes by some amount. Its best not
to use this in conjunction with the smoothed density plot because the
smoothing lengths are recalculated whenever the particles are
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

The smoothed density plot and smoothing length routines have been
parallelised with OpenMP. To use more than one processor, either use
the OpenMP option in the Options menu or set the environment variable
OMP_NUM_THREADS to the number of processors to use.


### Reading extra particle properties

#### HDF5 snapshots

If you have a HDF5 snapshot with extra particle properties (ages,
metallicities etc), you can make it read them in by putting their
dataset names in the file

.gadgetviewer_settings/gadget_hdf5_extra_properties

in your home directory. Note that duplicate dataset names or names which
conflict with quantities which are always read (e.g. Mass, ID) will be
silently ignored. The default file contains

```
 Metallicity
 StarFormationRate
 Temperature
 Density
 InternalEnergy
```

The program will read these quantities for each type of particle in the
snapshot which has a corresponding HDF5 dataset.


#### Type 2 binary snapshots

You can specify which blocks should be read from type 2 binary
snapshots by editing the file

.gadgetviewer_settings/gadget_binary_type2_blocks

in your home directory. The default file looks like this:

```
  U   , InternalEnergy,  T, F, F, F, F, F, REAL
  RHO , Density,         T, F, F, F, F, F, REAL
  HSML, SmoothingLength, T, F, F, F, F, F, REAL
```

There is one row for each additional block to be read. The columns are

  * Four character tag for this block
  * Name of the particle property stored in the block
  * Flags (T or F) which specify which particle types this property
    applies to
  * Data type - this should be REAL or INTEGER. The precision of the data
    (4 or 8 byte) is detected using the record markers in the file.

Note that vector quantities (e.g. accelerations) cannot be read in this
way. There is no need to specify the POS, VEL, ID and MASS blocks
because they are always read in anyway. Lines with names or tags that
conflict with the standard set of blocks will be ignored.

#### Type 1 binary snapshots

Its not possible to read additional particle properties from type 1
snapshots because there is no way for the program to determine which
extra blocks are present.


### Reading additional arrays from auxiliary files

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

