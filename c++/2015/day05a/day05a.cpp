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

bool is_vowel(char ch)
{
    return ch == 'a' || ch == 'e' || ch == 'i' || ch == 'o' || ch == 'u';
}

bool prop1(std::string str)
{
    uint32_t vowels = 0;
    for ( auto ch = str.begin(); ch != str.end(); ++ch ) {
        if ( is_vowel(*ch) ) {
            ++vowels;
        }
    }
    return vowels >= 3;
}

bool prop2(std::string str)
{
    for ( auto ch = 'a'; ch <= 'z'; ++ch ) {
        std::string double_ch;
        double_ch.push_back(ch);
        double_ch.push_back(ch);
        if ( str.find(double_ch) != std::string::npos ) {
            return true;
        }
    }
    return false;
}

bool prop3(std::string str)
{
    return str.find("ab") == std::string::npos &&
        str.find("cd") == std::string::npos &&
        str.find("pq") == std::string::npos &&
        str.find("xy") == std::string::npos;
}

uint32_t process(std::string filename)
{
    uint32_t count = 0;
    std::ifstream infile(filename);
    std::string line;
    while ( std::getline(infile, line) ) {
        if ( prop1(line) && prop2(line) && prop3(line) ) {
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
