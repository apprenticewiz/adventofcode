#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <string>

void usage(std::string progname)
{
    std::cerr << "usage: " << progname << " <input file>" << std::endl;
    std::exit(1);
}

bool prop1(std::string str)
{
    for ( auto ch1 = str.begin(); ch1 != str.end() - 3; ++ch1 ) {
        std::string first;
        first.push_back(*ch1);
        first.push_back(*(ch1 + 1));
        for ( auto ch2 = ch1 + 2; ch2 != str.end(); ++ch2 ) {
            std::string second;
            second.push_back(*ch2);
            second.push_back(*(ch2 + 1));
            if ( first == second ) {
                return true;
            }
        }
    }
    return false;
}

bool prop2(std::string str)
{
    for ( auto ch = str.begin(); ch != str.end() - 2; ++ch ) {
        if ( *ch == *(ch + 2) ) {
            return true;
        }
    }
    return false;
}

uint32_t process(std::string filename)
{
    uint32_t count = 0;
    std::ifstream infile(filename);
    std::string line;
    while ( std::getline(infile, line) ) {
        if ( prop1(line) && prop2(line) ) {
            ++count;
        }        
    }
    return count;
}

int main(int argc, char* argv[])
{
    std::string progname(argv[0]);
    if ( argc < 2 ) {
        usage(progname);
    }

    std::string filename(argv[1]);
    uint32_t result = process(filename);
    std::cout << "result = " << result << std::endl;
    return 0;
}
