#ifndef __POSITION_H
#define __POSITION_H

#include <cstdint>
#include <functional>

namespace aoc_utils {
template <class num_type>
struct Position2D {
  num_type x;
  num_type y;

  bool operator==(const struct Position2D<num_type>& other) const {
    return x == other.x && y == other.y;
  }
};

template <class num_type>
struct Position2DHasher {
  std::size_t operator()(const struct Position2D<num_type>& p) const {
    uint64_t px = static_cast<uint64_t>(p.x);
    uint64_t py = static_cast<uint64_t>(p.y);
    return std::hash<uint64_t>()(px) ^ (std::hash<uint64_t>()(py) << 1);
  }
};

template <class num_type>
struct Position3D {
  num_type x;
  num_type y;
  num_type z;

  bool operator==(const struct Position3D<num_type>& other) const {
    return x == other.x && y == other.y && z == other.z;
  }
};

template <class num_type>
struct Position3DHasher {
  std::size_t operator()(const struct Position3D<num_type>& p) const {
    uint64_t px = static_cast<uint64_t>(p.x);
    uint64_t py = static_cast<uint64_t>(p.y);
    uint64_t pz = static_cast<uint64_t>(p.z);
    return std::hash<uint64_t>()(px) ^ (std::hash<uint64_t>()(py) << 1) ^
           (std::hash<uint64_t>()(pz));
  }
};
}  // namespace aoc_utils

#endif
