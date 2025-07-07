#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <string>
#include <sstream>

#include "utils/md5.h"

void usage(std::string progname) {
    std::cerr << "usage: " << progname << " <key>" << std::endl;
    std::exit(1);
}

uint32_t process(std::string key) {
    uint32_t n = 1;
    for ( ; ; ) {
        std::ostringstream key_builder;
        key_builder << key << n;
        std::string try_key = key_builder.str();
	std::string digest = aoc_utils::md5(try_key);
        if ( digest.find("00000") == 0 ) {
            break;
        } else {
            ++n;
        }
    }
    return n;
}

int main(int argc, char* argv[]) {
    std::string progname(argv[0]);
    if ( argc < 2 ) {
        usage(progname);
    }
    std::string key(argv[1]);
    uint32_t result = process(key);
    std::cout << "result = " << result << std::endl;
    return 0;
}
