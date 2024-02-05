#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <sstream>
#include <unordered_set>

void usage(const std::string& progname) {
    std::cout << "usage: " << progname << " <file>\n";
    std::exit(1);
}

uint32_t process(const std::string& contents) {
    std::istringstream iss(contents);
    std::string line;
    uint32_t result = 0;
    while ( std::getline(iss, line) ) {
        size_t pos = line.find(": ");
        if ( pos == std::string::npos ) {
            break;
        }
        std::string rest = line.substr(pos + 2);
        pos = rest.find(" | ");
        if ( pos == std::string::npos ) {
            break;
        }
        std::string winning_str = rest.substr(0, pos);
        std::istringstream winning_iss{winning_str};
        std::unordered_set<uint32_t> winning_set;
        while ( !winning_iss.eof() ) {
            uint32_t num;
            winning_iss >> num;
            winning_set.insert(num);
        }
        std::string hand_str = rest.substr(pos + 2);
        std::istringstream hand_iss{hand_str};
        std::unordered_set<uint32_t> hand_set;
        while ( !hand_iss.eof() ) {
            uint32_t num;
            hand_iss >> num;
            hand_set.insert(num);
        }
        uint32_t common_count = 0;
        for ( auto elemp = winning_set.begin(); elemp != winning_set.end(); elemp++ ) {
            if ( hand_set.find(*elemp) != hand_set.end() ) {
                common_count++;
            }
        }
        result += (common_count > 0) ? (1 << (common_count - 1)) : 0;
    }
    return result;
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
