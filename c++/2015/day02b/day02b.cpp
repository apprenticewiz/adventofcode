#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

void usage(std::string progname)
{
    std::cerr << "usage: " << progname << " <input file>" << std::endl;
    std::exit(1);
}

uint32_t process(std::string filename)
{
    uint32_t total_len = 0;
    std::ifstream infile(filename);
    std::string line;
    while ( std::getline(infile, line) ) {
        std::stringstream ss(line);
        std::string token;
        std::vector<uint32_t> dimensions;
        while ( std::getline(ss, token, 'x') ) {
            uint32_t number = static_cast<uint32_t>(std::stoul(token));
            dimensions.push_back(number);
        }
        uint32_t perim1 = (dimensions[0] + dimensions[1]) << 1;
        uint32_t perim2 = (dimensions[0] + dimensions[2]) << 1;
        uint32_t perim3 = (dimensions[1] + dimensions[2]) << 1;
        uint32_t present_len = std::min(std::min(perim1, perim2), perim3);
        uint32_t bow_len = dimensions[0] * dimensions[1] * dimensions[2];
        total_len += present_len + bow_len;
    }
    return total_len;
}

int main(int argc, char* argv[])
{
    std::string progname(argv[0]);
    if ( argc < 2 ) {
        usage(progname);
    }

    std::string filename(argv[1]);
    int32_t result = process(filename);
    std::cout << "result = " << result << std::endl;
    return 0;
}