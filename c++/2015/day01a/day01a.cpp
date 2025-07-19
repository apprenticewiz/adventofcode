#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <string>

void usage(std::string progname) {
  std::cerr << "usage: " << progname << " <input file>" << std::endl;
  std::exit(1);
}

int32_t process(std::string filename) {
  int32_t counter = 0;
  std::ifstream infile(filename);
  std::string line;
  while (std::getline(infile, line)) {
    for (auto ch = line.begin(); ch != line.end(); ch++) {
      switch (*ch) {
        case '(':
          ++counter;
          break;
        case ')':
          --counter;
          break;
      }
    }
  }
  return counter;
}

int main(int argc, char* argv[]) {
  std::string progname(argv[0]);
  if (argc < 2) {
    usage(progname);
  }

  std::string filename(argv[1]);
  int32_t result = process(filename);
  std::cout << "result = " << result << std::endl;
  return 0;
}
