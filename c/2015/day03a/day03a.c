#include <stdio.h>
#include <stdlib.h>

#include "string_set.h"

typedef struct position {
    int x;
    int y;
} position_t;

void usage(char *progname) {
    fprintf(stderr, "usage: %s <input file>\n", progname);
    exit(1);
}

int process(char *filename) {
    FILE *infile;
    char ch;
    string_set_t *positions = string_set_new();
    position_t santa = { 0, 0 };
    char buf[16];
    int size;
  
    if ( (infile = fopen(filename, "r")) == NULL ) {
        perror("failed to open input file");
        exit(1);
    }
    while ( !feof(infile) ) {
        ch = (char)fgetc(infile);
        switch ( ch ) {
            case '^':
                santa.y += 1;
                break;
            case 'v':
                santa.y -= 1;
                break;
            case '<':
                santa.x -= 1;
                break;
            case '>':
                santa.x += 1;
                break;
        }
        sprintf(buf, "%d,%d", santa.x, santa.y);
        string_set_insert(positions, buf);
    }
    fclose(infile);
    size = string_set_size(positions);
    string_set_free(positions);
    return size;
}

int main(int argc, char *argv[]) {
    char *filename;
    int result;

    if ( argc < 2 ) {
        usage(argv[0]);
    }

    filename = argv[1];
    result = process(filename);
    printf("result = %d\n", result);
    return 0;
}
