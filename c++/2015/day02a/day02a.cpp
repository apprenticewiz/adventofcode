#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

void usage(std::string progname) {
  std::cerr << "usage: " << progname << " <input file>" << std::endl;
  std::exit(1);
}

uint32_t process(std::string filename) {
  uint32_t total_area = 0;
  std::ifstream infile(filename);
  std::string line;
  while (std::getline(infile, line)) {
    std::stringstream ss(line);
    std::string token;
    std::vector<uint32_t> dimensions;
    while (std::getline(ss, token, 'x')) {
      uint32_t number = static_cast<uint32_t>(std::stoul(token));
      dimensions.push_back(number);
    }
    uint32_t area1 = dimensions[0] * dimensions[1];
    uint32_t area2 = dimensions[0] * dimensions[2];
    uint32_t area3 = dimensions[1] * dimensions[2];
    uint32_t surface_area = (area1 << 1) + (area2 << 1) + (area3 << 1);
    uint32_t min_area = std::min(std::min(area1, area2), area3);
    total_area += surface_area + min_area;
  }
  return total_area;
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
