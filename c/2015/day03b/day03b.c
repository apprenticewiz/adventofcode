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
    position_t robo_santa = { 0, 0 };
    position_t current_pos;
    int santa_move = 1;
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
                santa_move ? (santa.y += 1) : (robo_santa.y += 1);
                break;
            case 'v':
                santa_move ? (santa.y -= 1) : (robo_santa.y -= 1);
                break;
            case '<':
                santa_move ? (santa.x -= 1) : (robo_santa.x -= 1);
                break;
            case '>':
                santa_move ? (santa.x += 1) : (robo_santa.x += 1);
                break;
        }
        current_pos = santa_move ? santa : robo_santa;
	sprintf(buf, "%d,%d", current_pos.x, current_pos.y);
        string_set_insert(positions, buf);
	santa_move = !santa_move;
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
