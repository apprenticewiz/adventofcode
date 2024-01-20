#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <map>
#include <sstream>
#include <utility>

typedef std::pair<int, int> Position;

void usage(const std::string& progname) {
    std::cout << "usage: " << progname << " <file>\n";
    std::exit(1);
}

std::map<Position, std::string> build_numbers(const std::string& contents) {
    std::istringstream iss(contents);
    std::string line;
    std::map<Position, std::string> number_locs;
    int row = 0;
    int col;
    char ch;
    bool scanning_number = false;
    Position current_pos;
    std::string number;
    while ( std::getline(iss, line) ) {
        for ( col = 0; col < line.size(); col++ ) {
            ch = line[col];
            if ( scanning_number ) {
                if ( std::isdigit(ch) ) {
                    number.push_back(ch);
                } else {
                    number_locs.insert({current_pos, number});
                    scanning_number = false;
                    number.clear();
                }
            } else {
                if ( std::isdigit(ch) ) {
                    number.push_back(ch);
                    current_pos = std::make_pair(row, col);
                    scanning_number = true;
                }
            }
        }
        if ( scanning_number ) {
            number_locs.insert({current_pos, number});
            scanning_number = false;
            number.clear();
        }
        row++;
    }
    return number_locs;
}

std::map<Position, char> build_gears(const std::string& contents) {
    std::istringstream iss(contents);
    std::string line;
    std::map<Position, char> gear_locs;
    int row = 0;
    int col;
    char ch;
    while ( std::getline(iss, line) ) {
        for ( col = 0; col < line.size(); col++ ) {
            ch = line[col];
            if ( !std::isdigit(ch) && ch != '.' ) {
                gear_locs.insert({std::make_pair(row, col), ch});
            }
        }
        row++;
    }
    return gear_locs;
}

uint32_t check_gears(const std::map<Position, std::string>& number_locs, const std::map<Position, char>& gear_locs) {
    uint32_t result = 0;
    for ( auto numlocp = number_locs.begin(); numlocp != number_locs.end(); numlocp++ ) {
        int adjacent_count = 0;
        int number_col_start = numlocp->first.second;
        int number_col_end = numlocp->first.second + numlocp->second.size();
        for ( int number_col = number_col_start; number_col < number_col_end; number_col++ ) {
            for ( int delta_row = -1; delta_row <= 1; delta_row ++ ) {
                int adjacent_row = numlocp->first.first + delta_row;
                for ( int delta_col = -1; delta_col <= 1; delta_col++ ) {
                    int adjacent_col = number_col + delta_col;
                    for ( auto gearlocp = gear_locs.begin(); gearlocp != gear_locs.end(); gearlocp++ ) {
                        if ( adjacent_row == gearlocp->first.first && adjacent_col == gearlocp->first.second ) {
                            adjacent_count++;
                        }
                    }
                }
            }
        }
        if ( adjacent_count != 0 ) {
            result += std::atoi(numlocp->second.c_str());
        }
    }
    return result;
}

uint32_t process(const std::string& contents) {
    std::map<Position, std::string> number_locs = build_numbers(contents);
    std::map<Position, char> gear_locs = build_gears(contents);
    return check_gears(number_locs, gear_locs);
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
