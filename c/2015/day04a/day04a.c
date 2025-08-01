#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "md5.h"

void usage(char *progname) {
    fprintf(stderr, "usage: %s <input file>\n", progname);
    exit(1);
}

int process(char *key) {
    char try_key[32];
    char digest[33];
    int n = 1;

    for ( ; ; ) {
        sprintf(try_key, "%s%d", key, n);
	md5_digest(try_key, digest);
        if ( strncmp(digest, "00000", 5) == 0 ) {
            return n;
        }
	n++;
    }
  
}

int main(int argc, char *argv[]) {
    char *key;
    int result;

    if ( argc < 2 ) {
        usage(argv[0]);
    }

    key = argv[1];
    result = process(key);
    printf("result = %d\n", result);
    return 0;
}
