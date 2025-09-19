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
    uint32_t enc_len = 0;
    for (auto i = 0; i < line.length(); i++) {
      switch (line[i]) {
        case '\\':
        case '\"':
          enc_len += 2;
          break;
        default:
          enc_len++;
      }
    }
    result += 2 + (enc_len - code_len);
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
