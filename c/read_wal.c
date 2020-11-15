// read_wal.c -- read a WAL image file in C.
// This file is distributed as part of the 'wal' R package repo and is covered by its license.
//
// Compilation and usage:
//
//    gcc read_wal.c
//    ./a.out path/to/some_image.wal
//
// Written by Tim Schaefer, 2020-11-14.


#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[]) {
    FILE *fh;

    if ((fh = fopen(argv[1], "r")) == NULL) {
      printf("Could not open file %s for reading\n", argv[1]);
      exit(1);
    }

    unsigned char tex_name[32];
    fread(&tex_name, sizeof(tex_name), 1, fh);

    int width, height;
    fread(&width, sizeof(int), 1, fh);
    fread(&height, sizeof(int), 1, fh);

    int mip_level_offsets[4];
    fread(&mip_level_offsets, sizeof(mip_level_offsets), 1, fh);

    unsigned char anim_name[32];
    fread(&anim_name, sizeof(anim_name), 1, fh);

    int flags, contents, value;
    fread(&flags, sizeof(int), 1, fh);
    fread(&contents, sizeof(int), 1, fh);
    fread(&value, sizeof(int), 1, fh);

    printf("The tex name is %s\n", tex_name);
    printf("The width is %d\n", width);
    printf("The height is %d\n", height);
    printf("The mip level offsets are: %d, %d, %d, %d\n", mip_level_offsets[0], mip_level_offsets[1], mip_level_offsets[2], mip_level_offsets[3]);
    printf("The anim name is %s\n", anim_name);
    printf("The flags is %d\n", flags);
    printf("The contents is %d\n", contents);
    printf("The value is %d\n", value);

    // Read image data. Each entry represents a pixel and is an index (in range 0-255) into the color palette, which is not part of the file.
    // A WAL file contains several image resolutions (mipmaps), the following code reads the first (i.e., the highest-quality) image version.
    int pixel_data[width * height];
    fseek(fh, mip_level_offsets[0], SEEK_SET);
    fread(&pixel_data, sizeof(pixel_data), 1, fh);

    // The pixel data should be reshaped to a matrix of width x height to use it. We don't do that here.

    fclose(fh);
}
