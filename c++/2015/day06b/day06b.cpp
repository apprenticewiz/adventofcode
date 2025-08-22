#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <regex>
#include <string>

#include "position.h"

const uint32_t ROW_MAX = 1000;
const uint32_t COL_MAX = 1000;

typedef struct aoc_utils::Position2D<int32_t> Position;

typedef struct Bounds {
  Position upper_left;
  Position lower_right;
} Bounds;

typedef class Grid {
private:
  uint32_t grid[ROW_MAX][COL_MAX];

public:
  Grid();
  void perform(const std::string&, const Bounds&);
  uint32_t count() const;
} Grid;

Grid::Grid() {
  for ( auto row = 0; row < ROW_MAX; row++ ) {
    for ( auto col = 0; col < COL_MAX; col++ ) {
      grid[row][col] = 0;
    }
  }
}

void Grid::perform(const std::string& action, const Bounds& bounds) {
  for ( auto row = bounds.upper_left.y; row <= bounds.lower_right.y; row++ ) {
    for ( auto col = bounds.upper_left.x; col <= bounds.lower_right.x; col++ ) {
      if ( action == "turn on" ) {
        grid[row][col] += 1;
      } else if ( action == "turn off" ) {
        grid[row][col] = (grid[row][col] > 0) ? (grid[row][col] - 1) : grid[row][col];
      } else if ( action == "toggle" ) {
        grid[row][col] += 2;
      }
    }
  }
}

uint32_t Grid::count() const {
  uint32_t count = 0;
  for ( auto row = 0; row < ROW_MAX; row++ ) {
    for ( auto col = 0; col < COL_MAX; col++ ) {
        count += grid[row][col];
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
