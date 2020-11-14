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

    // Could read data here.

    fclose(fh);
}
