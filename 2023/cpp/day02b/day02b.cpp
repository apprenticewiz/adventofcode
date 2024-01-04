#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <numeric>
#include <sstream>
#include <vector>

void usage(const std::string& progname) {
    std::cout << "usage: " << progname << " <file>\n";
    std::exit(1);
}

std::string trim_left(const std::string& s) {
    auto start = std::find_if_not(s.begin(), s.end(), [](int c) {
        return std::isspace(c);
    });

    return std::string(start, s.end());
}

uint32_t process(const std::string& contents) {
    std::vector<uint32_t> powers;
    std::istringstream iss(contents);
    std::string line;
    while ( std::getline(iss, line) ) {
        size_t pos = line.find(": ");
        uint32_t red_needed = 0;
        uint32_t green_needed = 0;
        uint32_t blue_needed = 0;
        if (pos != std::string::npos) {
            std::string game_str = line.substr(0, pos);
            std::string reveals_str = line.substr(pos + 2);
            pos = game_str.find(' ');
            if ( pos != std::string::npos ) {
                std::string game_num_str = game_str.substr(pos + 1);
                uint32_t game_num = std::stoi(game_num_str);
                std::istringstream reveals_iss(reveals_str);
                std::string subset_str;
                while ( std::getline(reveals_iss, subset_str, ';') ) {
                    std::istringstream cubes_iss(subset_str);
                    std::string cubes_str;
                    while ( std::getline(cubes_iss, cubes_str, ',') ) {
                        cubes_str = trim_left(cubes_str);
                        pos = cubes_str.find(' ');
                        if ( pos != std::string::npos ) {
                            std::string amount_str = cubes_str.substr(0, pos);
                            std::string color = cubes_str.substr(pos + 1);
                            uint32_t amount = std::stoi(amount_str);
                            if ( color == "red" && amount > red_needed ) {
                                red_needed = amount;
                            } else if ( color == "green" && amount > green_needed ) {
                                green_needed = amount;
                            } else if ( color == "blue" && amount > blue_needed ) {
                                blue_needed = amount;
                            }
                        }
                    }
                }
                powers.push_back(red_needed * green_needed * blue_needed);
            }
        }
    }
    return std::accumulate(powers.begin(), powers.end(), 0);
}

int main(int argc, char* argv[]) {
    if ( argc < 2 ) {
        usage(argv[0]);
    }

    std::string filename = argv[1];
    std::ifstream file(filename);
    if ( !file.is_open() ) {
        std::cerr << "read of input file failed\n";
        std::exit(1);
    }

    std::ostringstream buffer;
    buffer << file.rdbuf();
    std::string contents = buffer.str();

    uint32_t result = process(contents);
    std::cout << "result = " << result << "\n";

    return 0;
}