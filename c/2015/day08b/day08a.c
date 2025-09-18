#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void usage(char *progname) {
    fprintf(stderr, "usage: %s <input file>\n", progname);
    exit(1);
}

int process(char *filename) {
    FILE *infile;
    char *line;
    size_t size;
    int result = 0;
    int code_len, enc_len;
    int i;
  
    if ( (infile = fopen(filename, "r")) == NULL ) {
        perror("failed to open input file");
        exit(1);
    }
    line = NULL;
    while ( getline(&line, &size, infile) != -1 ) {
        code_len = strlen(line);
        enc_len = 0;
        for ( i = 0; i < strlen(line); i++ ) {
            switch ( line[i] ) {
                case '\\':
                case '\"':
                    enc_len += 2;
                    break;
                default:
                    enc_len++;
            }
        }
        free(line);
        line = NULL;
        result += 2 + (enc_len - code_len);
    }
    fclose(infile);
    return result;
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
