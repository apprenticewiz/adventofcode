#define _GNU_SOURCE
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ROW_MAX 1000
#define COL_MAX 1000

#define ACTION_UNKNOWN  0
#define ACTION_TURN_ON  1
#define ACTION_TURN_OFF 2
#define ACTION_TOGGLE   3

void usage(char *progname) {
    fprintf(stderr, "usage: %s <input file>\n", progname);
    exit(1);
}

void grid_init(int grid[ROW_MAX][COL_MAX])
{
    int i, j;

    for ( j = 0; j < ROW_MAX; j++ ) {
        for ( i = 0; i < COL_MAX; i++ ) {
            grid[j][i] = false;
        }
    }
}

void grid_perform(int grid[ROW_MAX][COL_MAX], int action, int r1, int c1, int r2, int c2) {
    int i, j;

    for ( j = r1; j <= r2; j++ ) {
        for ( i = c1; i <= c2; i++ ) {
            switch ( action ) {
            case ACTION_TURN_ON:
                grid[j][i] = true;
                break;
            case ACTION_TURN_OFF:
                grid[j][i] = false;
                break;
            case ACTION_TOGGLE:
                grid[j][i] = !grid[j][i];
                break;
            }
        }
    }
}

int grid_count(int grid[ROW_MAX][COL_MAX]) {
    int i, j;
    int count = 0;

    for ( j = 0; j < ROW_MAX; j++ ) {
        for ( i = 0; i < COL_MAX; i++ ) {
            if ( grid[j][i] ) {
                count++;
            }
        }
    }
    return count;
}

int process(char *filename) {
    FILE *infile;
    char *line;
    size_t size;
    int grid[ROW_MAX][COL_MAX];
    int action;
    char *p;
    int r1, c1, r2, c2;
  
    grid_init(grid);
    if ( (infile = fopen(filename, "r")) == NULL ) {
        perror("failed to open input file");
        exit(1);
    }
    line = NULL;
    while ( getline(&line, &size, infile) != -1 ) {
        if ( strstr(line, "turn on") != NULL ) {
            action = ACTION_TURN_ON;
        } else if ( strstr(line, "turn off") != NULL ) {
            action = ACTION_TURN_OFF;
        } else if ( strstr(line, "toggle") != NULL ) {
            action = ACTION_TOGGLE;
        } else {
            action = ACTION_UNKNOWN;
        }
        for ( p = line; !isdigit(*p); p++ );
	sscanf(p, "%d%*c%d%*s%d%*c%d", &r1, &c1, &r2, &c2);
	grid_perform(grid, action, r1, c1, r2, c2);
        free(line);
        line = NULL;
    }
    fclose(infile);
    return grid_count(grid);
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
