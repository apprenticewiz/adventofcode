#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <regex>
#include <string>

#include "position.h"

const uint32_t X_MAX = 1000;
const uint32_t Y_MAX = 1000;

typedef struct aoc_utils::Position2D<int32_t> Position;

typedef struct Bounds {
  Position upper_left;
  Position lower_right;
} Bounds;

typedef class Grid {
private:
  uint32_t grid[X_MAX][Y_MAX];

public:
  Grid();
  void perform(const std::string&, const Bounds&);
  uint32_t count() const;
} Grid;

Grid::Grid() {
  for ( auto j = 0; j < Y_MAX; j++ ) {
    for ( auto i = 0; i < X_MAX; i++ ) {
      grid[j][i] = 0;
    }
  }
}

void Grid::perform(const std::string& action, const Bounds& bounds) {
  for ( auto j = bounds.upper_left.y; j <= bounds.lower_right.y; j++ ) {
    for ( auto i = bounds.upper_left.x; i <= bounds.lower_right.x; i++ ) {
      if ( action == "turn on" ) {
        grid[j][i] += 1;
      } else if ( action == "turn off" ) {
        grid[j][i] = (grid[j][i] > 0) ? (grid[j][i] - 1) : grid[j][i];
      } else if ( action == "toggle" ) {
        grid[j][i] += 2;
      }
    }
  }
}

uint32_t Grid::count() const {
  uint32_t count = 0;
  for ( auto j = 0; j < Y_MAX; j++ ) {
    for ( auto i = 0; i < X_MAX; i++ ) {
        count += grid[j][i];
    }
  }
  return count;
}

void usage(std::string progname) {
  std::cerr << "usage: " << progname << " <input file>" << std::endl;
  std::exit(1);
}

uint32_t process(std::string filename) {
  Grid grid;
  std::ifstream infile(filename);
  std::string line;
  const std::regex re(R"((turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+))");
  std::smatch match;
  std::string action;
  Bounds bounds;
  while (std::getline(infile, line)) {
    if ( std::regex_match(line, match, re) ) {
      action = match[1].str();
      bounds.upper_left.x = std::stoul(match[2].str());
      bounds.upper_left.y = std::stoul(match[3].str());
      bounds.lower_right.x = std::stoul(match[4].str());
      bounds.lower_right.y = std::stoul(match[5].str());
      grid.perform(action, bounds);
    }
  }
  return grid.count();
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
