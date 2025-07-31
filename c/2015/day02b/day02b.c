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
    int perim1, perim2, perim3;
    int present_len, bow_len;
    int total_len = 0;
  
    if ( (infile = fopen(filename, "r")) == NULL ) {
        perror("failed to open input file");
        exit(1);
    }
    while ( fscanf(infile, "%dx%dx%d", &l, &w, &h) != -1 ) {
	perim1 = 2 * (l + w);
	perim2 = 2 * (l + h);
	perim3 = 2 * (w + h);
	present_len = (perim1 < perim2) ? ((perim1 < perim3) ? perim1 : perim3) :
	    ((perim2 < perim3) ? perim2 : perim3);
        bow_len = l * w * h;
        total_len += present_len + bow_len;
    }
    fclose(infile);
    return total_len;
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
