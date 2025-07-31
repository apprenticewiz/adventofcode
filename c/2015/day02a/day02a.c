#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void usage(char *progname) {
    fprintf(stderr, "usage: %s <input file>\n", progname);
    exit(1);
}

int process(char *filename) {
    FILE *infile;
    int l, w, h;
    int area1, area2, area3;
    int surface_area, min_area;
    int total_area = 0;
  
    if ( (infile = fopen(filename, "r")) == NULL ) {
        perror("failed to open input file");
        exit(1);
    }
    while ( fscanf(infile, "%dx%dx%d", &l, &w, &h) != -1 ) {
	area1 = l * w;
	area2 = l * h;
	area3 = w * h;
	surface_area = 2 * area1 + 2 * area2 + 2 * area3;
	min_area = (area1 < area2) ? ((area1 < area3) ? area1 : area3) : ((area2 < area3) ? area2 : area3);
	total_area += surface_area + min_area;
    }
    fclose(infile);
    return total_area;
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
