# wal
Read bitmap images in WAL file format in R.

## About

The WAL file format is an old format for storing indexed bitmap images, used as textures in idtech1 and idtech2 games. Note that whe WAL textures are stored inside Quake pak file archives (`pakn.pak`), so you will not see any WAL files in your Quake 2 directory unless you extracted them for mapping or modding.

WAL format files are indexed but do **not** contain the palette needed to reconstruct the image. You can use your own palette, or use the default palette that comes with this package (the Quake 2 palette). 

## Package API

### Usage: Reading and displaying WAL textures in R

    wal_file = "~/path/to/q2_pak0_extracted/textures/e1u2/basic1_7.wal";
    wal = wal::read.wal(wal_file);
    plot(wal, palette = wal::pal_q2());

### Return value details:

The `wal` instance above is a named list, the entry `image` contains an integer array with dimension `image_width x image_height x 3`. The last dimension represents the three RGB channels. The values are in range 0-255 and have been generated by applying a palette. You can create your own custom palette or use the provided Quake 1 or Quake 2 palettes (functions `wal::pal_q1()` and `wal::pal_q2()`, respectively). 

### Export

    wal::wal.export.to.jpeg(wal, "~/mytex.jpg", palette = wal::pal_q1(), quality = 0.95);
    wal::wal.export.to.png(wal, "~/mytex.png");          # palette defaults to pal_q2().

## References

Documentation on the WAL format seems sparse. I did not find any official spec so I used the Quake 2 source code to learn about the format. Relevant files are:

* [r_image.c](https://github.com/id-Software/Quake-2/blob/master/ref_soft/r_image.c) with WAL loading function `R_LoadWal`
* To understand the latter, you will need to have a look at the `miptex_s` struct in [qfiles.h](https://github.com/id-Software/Quake-2/blob/master/qcommon/qfiles.h) and at the `image_s` struct in [r_local.h](https://github.com/id-Software/Quake-2/blob/master/ref_soft/r_local.h)

## Unit tests and CI

[![Build Status](https://travis-ci.org/dfsp-spirit/wal.svg?branch=master)](https://travis-ci.org/dfsp-spirit/wal) Travis CI under Linux

[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/dfsp-spirit/wal?branch=master&svg=true)](https://ci.appveyor.com/project/dfsp-spirit/wal) AppVeyor CI under Windows


