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

    int width;
    fread(&width, sizeof(int), 1, fh);

    int height;
    fread(&height, sizeof(int), 1, fh);

    printf("The tex name is %s\n", tex_name);
    printf("The width is %d\n", width);
    printf("The height is %d\n", height);
}
