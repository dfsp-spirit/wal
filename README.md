# wal
Read bitmap images in WAL file format in R.

## About

The WAL file format is an old format for storing indexed bitmap images, used in idtech1 and idtech2 games. The WAL files that hold the Quake 2 textures are stored inside Quake pak file archives (similar to zip format), so you will not find any in your Quake 2 directory unless you extracted them for mapping or modding.


## Package API

Not yet, this is WIP. The relevant function is `wal::read.wal()` though.


## References

Documentation on the WAL format seems sparse. I did not find any official spec so I used the Quake 2 source code to learn about the format. Relevant files are:

* [r_image.c](https://github.com/id-Software/Quake-2/blob/master/ref_soft/r_image.c) with WAL loading function `R_LoadWal`
* To understand the latter, you will need to have a look at the `miptex_s` struct in [qfiles.h](https://github.com/id-Software/Quake-2/blob/master/qcommon/qfiles.h) and at the `image_s` struct in [r_local.h](https://github.com/id-Software/Quake-2/blob/master/ref_soft/r_local.h)

As such, this package reads *what Quake 2 considers* the WAL format.
