# wal
Read bitmap images in WAL file format in R.

## About

The WAL file format is an old format for storing indexed bitmap images, used in idtech1 and idtech2 games. The WAL files that hold the Quake 2 textures are stored inside Quake pak file archives (similar to zip format), so you will not find any in your Quake 2 directory unless you extracted them for mapping or modding.

## Package API

This is WIP.

## References

Documentation seems sparse. I did not find an official spec but some info is here:

* [Fileformats Wiki on WAL format](http://fileformats.archiveteam.org/wiki/Quake_2_Texture): Contains links to [Quake 2 source code on GitHub}(https://github.com/id-Software/Quake-2/blob/master/ref_soft/r_image.c) with WAL reading functions
