#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <string>

void usage(std::string progname) {
  std::cerr << "usage: " << progname << " <input file>" << std::endl;
  std::exit(1);
}

uint32_t process(std::string filename) {
  uint32_t result = 0;
  std::ifstream infile(filename);
  std::string line;
  while (std::getline(infile, line)) {
    uint32_t code_len = line.length();
    uint32_t mem_len = 0;
    for (auto i = 1; i < line.length() - 1;) {
      switch (line[i]) {
        case '\\':
          switch (line[i + 1]) {
            case '\\':
            case '\"':
              i += 2;
              break;
            case 'x':
              i += 4;
              break;
            default:
              i++;
          }
          break;
        default:
          i++;
      }
      mem_len++;
    }
    result += code_len - mem_len;
  }
  return result;
}

int main(int argc, char *argv[]) {
  std::string progname(argv[0]);
  if (argc < 2) {
    usage(progname);
  }
  std::string filename(argv[1]);
  uint32_t result = process(filename);
  std::cout << "result = " << result << std::endl;
  return 0;
}
