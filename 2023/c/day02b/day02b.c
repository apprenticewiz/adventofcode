#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

void usage(const char *progname) {
    printf("usage: %s <file>\n", progname);
    exit(EXIT_FAILURE);
}

void split_once(const char *source, char delim, char **first, char **second) {
    char *p = NULL;
    int pos = 0;

    if ( (p = strchr(source, delim)) != NULL ) {
        pos = p - source;
        if ( first != NULL ) {
            *first = strndup(source, pos);
        }
        if ( second != NULL ) {
            *second = strndup(source + pos + 1, strlen(source) - pos);
        }
    } else {
        if ( first != NULL ) {
            *first = NULL;
        }
        if ( second != NULL ) {
            *second = NULL;
        }
    }
}

uint32_t process(const char *filename) {
    uint32_t result = 0;
    char *line = NULL;
    FILE *infile = NULL;
    size_t len = 0;
    ssize_t nread = 0;
    char *game_part = NULL;
    char *draws_part = NULL;
    char *game_num_part = NULL;
    char *draw = NULL;
    char *rest = NULL;
    char *color_amount = NULL;
    char *color = NULL;
    char *amount_str = NULL;
    int amount = 0;
    int red_needed = 0, green_needed = 0, blue_needed = 0;
    int last_draw = 0, last_color_amount = 0;

    if ( (infile = fopen(filename, "r")) == NULL ) {
        perror("fopen");
        exit(EXIT_FAILURE);
    }
    while ( (nread = getline(&line, &len, infile)) != -1 ) {
        len = strlen(line);
        line[len - 1] = '\0';
        split_once(line, ':', NULL, &draws_part);
        last_draw = 0;
	red_needed = 0;
	green_needed = 0;
	blue_needed = 0;
        for ( ; ; ) {
            split_once(draws_part, ';', &draw, &rest);
            if ( draw != NULL ) {
                draws_part = rest;
            } else {
                draw = strdup(draws_part);
                last_draw = 1;
            }
            last_color_amount = 0;
            for ( ; ; ) {
                split_once(draw, ',', &color_amount, &rest);
                if ( color_amount != NULL ) {
                    draw = rest;
                } else {
                    color_amount = strdup(draw);
                    last_color_amount = 1;
                }
                split_once(color_amount + 1, ' ', &amount_str, &color);
                amount = atoi(amount_str);
                if ( strcmp(color, "red") == 0 && amount > red_needed ) {
                    red_needed = amount;
                } else if ( strcmp(color, "green") == 0 && amount > green_needed ) {
                    green_needed = amount;
                } else if ( strcmp(color, "blue") == 0 && amount > blue_needed ) {
                    blue_needed = amount;
                }
                if ( last_color_amount ) {
                    break;
                }
            }
            if ( last_draw ) {
                break;
            }
        }
        result += red_needed * green_needed * blue_needed;
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
