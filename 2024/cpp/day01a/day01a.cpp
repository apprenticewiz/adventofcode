#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

void usage(char *progname)
{
    std::cerr << "usage: " << progname << " <filename>" << std::endl;
    std::exit(1);
}

int processInput(const char *filename)
{
    std::ifstream infile(filename);
    if ( !infile.is_open() )
    {
        std::cerr << "error opening file: " << filename << std::endl;
        std::exit(1);
    }
    std::vector<int> firstList, secondList;
    std::string line;
    while ( std::getline(infile, line) )
    {
        std::stringstream buf(line);
        int firstNum, secondNum;
        buf >> firstNum >> secondNum;
        firstList.push_back(firstNum);
        secondList.push_back(secondNum);
    }
    std::sort(firstList.begin(), firstList.end());
    std::sort(secondList.begin(), secondList.end());
    int result = 0;
    for ( auto first = firstList.begin(), second = secondList.begin();
          first != firstList.end(), second != secondList.end();
          ++first, ++second)
    {
        result += std::abs(*first - *second);
    }
    return result;
}

int main(int argc, char *argv[])
{
    if ( argc < 2 )
    {
        usage(argv[0]);
    }
    int result = processInput(argv[1]);
    std::cout << "result = " << result << std::endl;
    return 0;
}
