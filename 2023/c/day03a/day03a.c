#include <ctype.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

typedef struct position {
    int row;
    int col;
} position_t;

typedef struct number_location {
    char *number;
    position_t pos;
} number_location_t;

typedef struct gear_location {
    char gear;
    position_t pos;
} gear_location_t;

/* C does not have extensible vectors like more modern languages, which is
 * rather annoying.  So instead, we use a home-grown linked list
 * implementation to provide similar functionality.
 */

typedef struct node {
    void *data;
    struct node *next;
} node_t;

void append_list(node_t **list, void* data) {
    node_t *curr = *list;
    node_t *temp  = (node_t *)malloc(sizeof(node_t));
    temp->data = data;
    temp->next = NULL;
    if ( *list == NULL ) {
        *list = temp;
    } else {
        while ( curr->next != NULL ) {
            curr = curr->next;
        }
        curr->next = temp;
    }
}

void free_list(node_t *list) {
    node_t *listptr = list;
    node_t *temp;
    while ( listptr != NULL ) {
        temp = listptr;
        listptr = listptr->next;
        free(temp->data);
        free(temp);
    }
}

void usage(const char *progname) {
    printf("usage: %s <file>\n", progname);
    exit(EXIT_FAILURE);
}

node_t *build_numbers(const char *filename) {
    char *line = NULL;
    FILE *infile = NULL;
    size_t len = 0;
    ssize_t nread = 0;
    node_t *numbers = NULL;
    number_location_t *numloc;
    char number[8];
    int numidx = 0;
    int row, col;
    int reading_number = 0;
    char ch;

    if ( (infile = fopen(filename, "r")) == NULL ) {
        perror("fopen");
        exit(EXIT_FAILURE);
    }
    row = 0;
    while ( (nread = getline(&line, &len, infile)) != -1 ) {
        len = strlen(line);
        for ( col = 0; col < len; col++ ) {
            ch = line[col];
            if ( reading_number ) {
                if ( isdigit(ch) ) {
                    number[numidx] = ch;
                    numidx++;
                } else {
                    number[numidx] = '\0';
                    numloc->number = strdup(number);
                    number[0] = '\0';
                    numidx = 0;
                    append_list(&numbers, (void *)numloc);
                    reading_number = 0;
                }
            } else {
                if ( isdigit(ch) ) {
                    number[numidx] = ch;
                    numidx++;
                    numloc = (number_location_t *)malloc(sizeof(number_location_t));
                    numloc->pos.row = row;
                    numloc->pos.col = col;
                    reading_number = 1;
                }
            }
        }
        row++;
    }
    fclose(infile);
    return numbers;
}

node_t *build_gears(const char *filename) {
    char *line = NULL;
    FILE *infile = NULL;
    size_t len = 0;
    ssize_t nread = 0;
    node_t *gears = NULL;
    gear_location_t *gear;
    int row, col;
    char ch;

    if ( (infile = fopen(filename, "r")) == NULL ) {
        perror("fopen");
        exit(EXIT_FAILURE);
    }
    row = 0;
    while ( (nread = getline(&line, &len, infile)) != -1 ) {
        len = strlen(line);
        for ( col = 0; col < len; col++ ) {
            ch = line[col];
            if ( !isdigit(ch) && ch != '.' && ch != '\n' ) {
                gear = (gear_location_t *)malloc(sizeof(gear_location_t));
                gear->gear = ch;
                gear->pos.row = row;
                gear->pos.col = col;
                append_list(&gears, (void *)gear);
            }
        }
        row++;
    }
    fclose(infile);
    return gears;
}

uint32_t check_gears(node_t *numbers, node_t *gears) {
    node_t *numberptr = numbers;
    number_location_t *numloc;
    node_t *gearptr;
    gear_location_t *gear;
    uint32_t result = 0;
    int adjacent_count;
    int len;
    int adj_row, adj_col;
    int num_col, delta_row, delta_col;
    while ( numberptr != NULL ) {
        numloc = (number_location_t *)numberptr->data;
        len = strlen(numloc->number);
        adjacent_count = 0;
        for ( num_col = numloc->pos.col; num_col < numloc->pos.col + len; num_col++ ) {
            for ( delta_row = -1; delta_row <= 1; delta_row++ ) {
                adj_row = numloc->pos.row + delta_row;
                for ( delta_col = -1; delta_col <= 1; delta_col++ ) {
                    adj_col = num_col + delta_col;
                    gearptr = gears;
                    while ( gearptr != NULL ) {
                        gear = (gear_location_t *)gearptr->data;
                        if ( adj_row == gear->pos.row && adj_col == gear->pos.col ) {
                            adjacent_count++;
                        }
                        gearptr = gearptr->next;
                    }
                }
            }
        }
        if ( adjacent_count != 0 ) {
            result += atoi(numloc->number);
        }
        numberptr = numberptr->next;
    }
    return result;
}

uint32_t process(const char *filename) {
    node_t *numbers = build_numbers(filename);
    node_t *gears = build_gears(filename);
    uint32_t result = check_gears(numbers, gears);
    free_list(numbers);
    free_list(gears);
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
