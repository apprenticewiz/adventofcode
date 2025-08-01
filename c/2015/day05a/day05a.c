#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void usage(char *progname) {
    fprintf(stderr, "usage: %s <input file>\n", progname);
    exit(1);
}

int prop1(char *str) {
    int vowels = 0;
    char *p;

    for ( p = str; *p != '\0'; p++ ) {
        if ( *p == 'a' || *p == 'e' || *p == 'i' || *p == 'o' || *p == 'u' ) {
            vowels++;
        }
    }
    return vowels >= 3;
}

int prop2(char *str) {
    char *p;

    for ( p = str + 1; *p != '\0'; p++ ) {
        if ( *p == *(p - 1) ) {
            return 1;
        }
    }
    return 0;
}

int prop3(char *str) {
    return strstr(str, "ab") == NULL && strstr(str, "cd") == NULL &&
        strstr(str, "pq") == NULL && strstr(str, "xy") == NULL;
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
        if ( prop1(line) && prop2(line) && prop3(line) ) {
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
