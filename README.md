# wal
Read, write, create and export bitmap images in WAL file format in R.

![Vis](./vignettes/Bricks050_256_Color.jpg?raw=true "Example Game texture.")

## About

The WAL file format is an old format for storing indexed bitmap images, used as textures in idtech1 and idtech2 games. A single WAL file contains four [mipmaps](https://en.wikipedia.org/wiki/Mipmap) of the same image. WAL format files are indexed but do **not** contain the palette needed to reconstruct the image. You can use your own palette, or use the Q1 or Q2 palettes that come with this package.

Note that whe WAL textures are stored inside Quake pak file archives (`baseq2/pak0.pak`), so you will not see any WAL files in your Quake 2 directory unless you extracted them for mapping or modding. For Quake I, they are stored directly in the BSP level files (which are inside `Id1/PAK0.PAK`), or in WAD files.


## Package features

* Reading WAL format images, including all mipmap levels. By default, the largest version is used.
* Visualization of WAL images in R, including all mipmap levels (requires a palette).
* Export of WAL images to PNG and JPEG format files (requires a palette).
* Two important palettes are included: the Q1 and Q2 palettes.
* Writing of a WAL image instance to a new file in WAL format.
* Converting other images (PNG, JPEG, whatever) to WAL instances and files, including:
  * Mapping the colors of the source image to the most similar colors of the target WAL palette. This is done in LAB color space using DeltaE as the similarity measure.
  * Generation of the mipmaps.


## Installation

I recommend to install the [stable wal version from CRAN](https://cran.r-project.org/package=wal):

    install.packages("wal");


To install the development version (no complaints or bug reports, please):

    install.packages("devtools");
    devtools::install_github('dfsp-spirit/wal')


## Package API

### Reading and displaying WAL textures in R

    wal_file = "~/path/to/q2_pak0_extracted/textures/e1u2/basic1_7.wal";
    wal = wal::read.wal(wal_file);
    
    plot(wal);  # S3 plot method for 'wal' instances, uses preview palette. Alternatively:
    wal::plotwal.mipmap(wal, mip_level = 1L, apply_palette = wal::pal_q2());  # Plot with full control.

### Exporting WAL textures to other formats
    
    wal::wal.export.to.jpeg(wal, "~/mytex.jpg", palette = wal::pal_q1(), quality = 0.95);
    wal::wal.export.to.png(wal, "~/mytex.png");          # palette defaults to pal_q2().
    
    
### Convert a JPEG or PNG image to WAL format

    jpeg_file = "~/mytextures/bricks_256x256.jpg";
    wal = img.to.wal(jpeg::readJPEG(jpeg_file));
    wal::writeWAL("~/myfile.wal", wal);

For more information, read the great vignette that comes with the package.

## References

Documentation on the WAL format seems sparse. I did not find any official spec so I used the Quake 2 source code to learn about the format. Relevant files are:

* [r_image.c](https://github.com/id-Software/Quake-2/blob/master/ref_soft/r_image.c) with WAL loading function `R_LoadWal`
* To understand the latter, you will need to have a look at the `miptex_s` struct in [qfiles.h](https://github.com/id-Software/Quake-2/blob/master/qcommon/qfiles.h) and at the `image_s` struct in [r_local.h](https://github.com/id-Software/Quake-2/blob/master/ref_soft/r_local.h)

## Unit tests and CI

[![Build Status](https://travis-ci.org/dfsp-spirit/wal.svg?branch=master)](https://travis-ci.org/dfsp-spirit/wal) Travis CI under Linux

[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/dfsp-spirit/wal?branch=master&svg=true)](https://ci.appveyor.com/project/dfsp-spirit/wal) AppVeyor CI under Windows


