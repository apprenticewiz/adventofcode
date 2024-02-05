#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <sstream>
#include <unordered_map>
#include <unordered_set>

void usage(const std::string& progname) {
    std::cout << "usage: " << progname << " <file>\n";
    std::exit(1);
}

uint32_t process(const std::string& contents) {
    std::istringstream iss(contents);
    std::string line;
    std::unordered_map<int, uint32_t> instances;
    uint32_t result = 0;
    while ( std::getline(iss, line) ) {
        size_t pos = line.find(": ");
        if ( pos == std::string::npos ) {
            break;
        }
        std::string card_part = line.substr(0, pos);
        std::string rest = line.substr(pos + 2);
        auto strp = card_part.begin();
        for ( pos = 4; pos < card_part.size(); pos++ ) {
            if ( std::isdigit(*(strp + pos)) ) {
                break;
            }
        }
        std::string card_num_str = card_part.substr(pos);
        uint32_t card_num = std::atoi(card_num_str.c_str());
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
        for ( int i = card_num + 1; i < card_num + common_count + 1; i++ ) {
            uint32_t copies = 0;
            if ( instances.find(i) != instances.end() ) {
                copies += instances[i];
            }
            copies++;
            if ( instances.find(card_num) != instances.end() ) {
                copies += instances[card_num];
            }
            instances[i] = copies;
        }
        result++;
    }
    for ( auto instancep = instances.begin(); instancep != instances.end(); instancep++ ) {
        result += instancep->second;
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
