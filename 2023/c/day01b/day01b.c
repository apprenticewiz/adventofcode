#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

char *strstrr(char *haystack, char *needle) {
    char *p = NULL;
    for ( p = haystack + strlen(haystack) - 1; p >= haystack; p-- ) {
        if ( strncmp(p, needle, strlen(needle)) == 0 ) {
            return p;
        }
    }
    return NULL;
}

void usage(const char *progname) {
    printf("usage: %s <file>\n", progname);
    exit(EXIT_FAILURE);
}

uint32_t process(const char *filename) {
    uint32_t result = 0;
    char *digits[] = { "0" , "1", "2", "3", "4", "5", "6", "7", "8", "9",
        "zero", "one", "two", "three", "four", "five", "six", "seven",
        "eight", "nine", NULL };
    char **digitp = NULL;
    char *line = NULL;
    FILE *infile = NULL;
    size_t len = 0;
    ssize_t nread = 0;
    char *ptr = NULL;
    int min_index = 0, max_index = 0;
    int left_index = 0, right_index = 0;
    int left_digit = 0, right_digit = 0;

    if ( (infile = fopen(filename, "r")) == NULL ) {
        perror("fopen");
        exit(EXIT_FAILURE);
    }
    while ( (nread = getline(&line, &len, infile)) != -1 ) {
        len = strlen(line);
        line[len - 1] = '\0';
        min_index = INT_MAX;
        max_index = INT_MIN;
        left_digit = 0;
        right_digit = 0;
        for ( digitp = digits; *digitp != NULL; digitp++ ) {
            if ( (ptr = strstr(line, *digitp)) != NULL ) {
                left_index = ptr - line;
                ptr = strstrr(line, *digitp);
                right_index = ptr - line;
                if ( left_index < min_index ) {
                    min_index = left_index;
                    left_digit = (digitp - digits) % 10;
                }
                if ( right_index > max_index ) {
                    max_index = right_index;
                    right_digit = (digitp - digits) % 10;
                } 
            }
        }
        result += left_digit * 10 + right_digit;
    }
    fclose(infile);
    return result;
}

int main(int argc, char *argv[]) {
    char *filename = NULL;
    uint32_t result = 0;

    if ( argc < 2 ) {
        usage(argv[0]);
    }
    filename = argv[1];
    result = process(filename);
    printf("result = %d\n", result);
    return EXIT_SUCCESS;
}
