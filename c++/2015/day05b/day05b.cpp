#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <regex>
#include <string>

void usage(std::string progname) {
  std::cerr << "usage: " << progname << " <input file>" << std::endl;
  std::exit(1);
}

bool prop1(std::string str) {
  std::regex re(R"((..).*\1)");
  return std::regex_search(str, re);
}

bool prop2(std::string str) {
  std::regex re(R"((.).\1)");
  return std::regex_search(str, re);
}

uint32_t process(std::string filename) {
  uint32_t count = 0;
  std::ifstream infile(filename);
  std::string line;
  while (std::getline(infile, line)) {
    if (prop1(line) && prop2(line)) {
      count++;
    }
  }
  return count;
}

int main(int argc, char* argv[]) {
  std::string progname(argv[0]);
  if (argc < 2) {
    usage(progname);
  }
  std::string filename(argv[1]);
  uint32_t result = process(filename);
  std::cout << "result = " << result << std::endl;
  return 0;
}
