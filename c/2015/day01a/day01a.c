#include <stdio.h>
#include <stdlib.h>

void usage(char *progname) {
    fprintf(stderr, "usage: %s <input file>\n", progname);
    exit(1);
}

int process(char *filename) {
    FILE *infile;
    char ch;
    int floor = 0;
  
    if ( (infile = fopen(filename, "r")) == NULL ) {
        perror("failed to open input file");
        exit(1);
    }
    while ( !feof(infile) ) {
        ch = (char)fgetc(infile);
        switch ( ch ) {
            case '(':
                floor++;
                break;
            case ')':
                floor--;
                break;
        }
    }
    fclose(infile);
    return floor;
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
