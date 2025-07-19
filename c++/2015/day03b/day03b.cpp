#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <string>
#include <unordered_set>

#include "utils/position.h"

typedef struct aoc_utils::Position2D<int32_t> Position;

typedef struct aoc_utils::Position2DHasher<int32_t> PositionHasher;

void usage(std::string progname) {
  std::cerr << "usage: " << progname << " <input file>" << std::endl;
  std::exit(1);
}

uint32_t process(std::string filename) {
  std::ifstream infile(filename);
  std::string line;
  Position santa{0, 0};
  Position robo_santa{0, 0};
  std::unordered_set<Position, PositionHasher> positions;
  positions.insert(santa);
  while (std::getline(infile, line)) {
    for (auto ch = line.begin(); ch != line.end(); ch += 2) {
      switch (*ch) {
        case '^':
          santa.y += 1;
          break;
        case 'v':
          santa.y -= 1;
          break;
        case '<':
          santa.x -= 1;
          break;
        case '>':
          santa.x += 1;
          break;
      }
      positions.insert(santa);
      switch (*(ch + 1)) {
        case '^':
          robo_santa.y += 1;
          break;
        case 'v':
          robo_santa.y -= 1;
          break;
        case '<':
          robo_santa.x -= 1;
          break;
        case '>':
          robo_santa.x += 1;
          break;
      }
      positions.insert(robo_santa);
    }
  }
  return static_cast<uint32_t>(positions.size());
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
