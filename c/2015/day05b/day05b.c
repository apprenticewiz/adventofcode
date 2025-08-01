#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void usage(char *progname) {
    fprintf(stderr, "usage: %s <input file>\n", progname);
    exit(1);
}

int prop1(char *str) {
    char first[3], second[3];
    int i, j;

    for ( i = 0; i < strlen(str) - 4; i++ ) {
        strncpy(first, str + i, 2);
	for ( j = i + 2; j < strlen(str) - 2; j++ ) {
	    strncpy(second, str + j, 2);
	    if ( strncmp(first, second, 2) == 0 ) {
		return 1;
	    }
	}
    }
    return 0;
}

int prop2(char *str) {
    char *p;

    for ( p = str + 2; *p != '\0'; p++ ) {
        if ( *p == *(p - 2) ) {
            return 1;
        }
    }
    return 0;
}

int process(char *filename) {
    FILE *infile;
    char *line;
    size_t size;
    int count = 0;

    if ( (infile = fopen(filename, "r")) == NULL ) {
        perror("failed to open input file");
        exit(1);
    }
    line = NULL;
    while ( getline(&line, &size, infile) != -1 ) {
        if ( prop1(line) && prop2(line) ) {
                count++;
        }
        free(line);
        line = NULL;
    }
    fclose(infile);
    return count;
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
