# wal
Tools to work with Quake game assets in [R](https://www.r-project.org/). Useful for messing with Quake and Quake II game textures. Can read, write, create and export bitmap images in WAL file format and related texture formats. Includes tools to extract WAD and PAK files. Can read Quake and Quake II models in MDL and MD2 formats and allows for exporting to standard mesh formats.

![Vis](./vignettes/Bricks050_256_Color.jpg?raw=true "Example Game texture.")

## About

The WAL file format is an old format for storing indexed bitmap images, used as textures in idtech1 and idtech2 games. A single WAL file contains four [mipmaps](https://en.wikipedia.org/wiki/Mipmap) of the same image. WAL format files are indexed but do **not** contain the palette needed to reconstruct the image. You can use your own palette, or use the Q1 or Q2 palettes that come with this package.

Note that whe WAL textures are stored inside Quake pak file archives (`baseq2/pak0.pak`), so you will not see any WAL files in your Quake 2 directory unless you extracted them for mapping or modding. For Quake I, textures in a very similar format that is also supported are stored directly in the BSP level files (which are inside `Id1/PAK0.PAK`), or in WAD files.


## Package features

Managing WAL images:

* Reading WAL format images, including all mipmap levels. By default, the largest version is used.
* Visualization of WAL images in R, including all mipmap levels (requires a palette).
* Export of WAL images to PNG and JPEG format files (requires a palette).
* Two important palettes are included: the Q1 and Q2 palettes.
* Writing of a WAL image instance to a new file in WAL format.
* Converting other images (PNG, JPEG, whatever) to WAL instances and files, including:
  * Mapping the colors of the source image to the most similar colors of the target WAL palette. This is done in LAB color space using DeltaE as the similarity measure.
  * Generation of the mipmaps.

Related Formats and Features:

* Reading WAD files in the 'WAD2' archive format used by Quake I and related games. These are tar-like archive files that hold collections of Quake assets (colormaps, textures in Q1 Mipmap Texture format, console pics, ...). Note that some games like Daikatana use a similar, but not identical format and still name their archives WAD files. These other WAD-like formats are currently not supported. Please open an issue if you need support for them.
* Extracting WAD archives to a directory.
* Reading, visualizing, and exporting Q1 Mipmap format textures. These are the textures used by Quake I, and they are typically stored directly in the binary maps (BSP files) or in WAD texture archives (see above). The Q1 Mipmap Texture format is very similar to the Quake II WAL format.
* Reading and extracting 'PAK' archives.
* Reading and exporting Quake models, including Quake MDL files and Quake II MD2 files. You can visualize them directly in R with 'rgl' or export them to standard model file formats like '.obj' format to open them in standard modeling software.


## Installation

I recommend to install the [stable wal version from CRAN](https://cran.r-project.org/package=wal). In your R session:

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

For more information, read the [detailed vignette](https://cran.r-project.org/web/packages/wal/vignettes/wal.html) that comes with the package. Examples for working with models and archives can be found in the [unit tests](./tests/testthat/).

## References

Documentation on the WAL format seems sparse. I did not find any official spec so I used the Quake 2 source code to learn about the format. Relevant files are:

* [r_image.c](https://github.com/id-Software/Quake-2/blob/master/ref_soft/r_image.c) with WAL loading function `R_LoadWal`
* To understand the latter, you will need to have a look at the `miptex_s` struct in [qfiles.h](https://github.com/id-Software/Quake-2/blob/master/qcommon/qfiles.h) and at the `image_s` struct in [r_local.h](https://github.com/id-Software/Quake-2/blob/master/ref_soft/r_local.h)

## Unit tests and CI

[![Build Status](https://travis-ci.org/dfsp-spirit/wal.svg?branch=master)](https://travis-ci.org/dfsp-spirit/wal) Travis CI under Linux

[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/dfsp-spirit/wal?branch=master&svg=true)](https://ci.appveyor.com/project/dfsp-spirit/wal) AppVeyor CI under Windows


