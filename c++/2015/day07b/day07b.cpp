#include <array>
#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <functional>
#include <iostream>
#include <map>
#include <regex>
#include <string>
#include <utility>
#include <variant>

using MatchHandler = std::function<void(const std::smatch&)>;

enum Operator {
  ASSIGN,
  NOT,
  AND,
  OR,
  LSHIFT,
  RSHIFT,
};

struct Unary {
  Operator op;
  std::string src;
};

struct Binary {
  Operator op;
  std::string src1;
  std::variant<std::string, int> src2;
};

using Operation = std::variant<Unary, Binary>;

void usage(std::string progname) {
  std::cerr << "usage: " << progname << " <input file>" << std::endl;
  std::exit(1);
}

int eval(const std::map<std::string, Operation>& operations,
         std::map<std::string, int>& cache, const std::string& expr) {
  if (std::regex_match(expr, std::regex(R"(\d+)"))) {
    return std::stoi(expr);
  }
  if (cache.find(expr) != cache.end()) {
    return cache[expr];
  }
  const Operation& op = operations.at(expr);
  int result = std::visit(
      [&](const auto& operation) -> int {
        using T = std::decay_t<decltype(operation)>;
        if constexpr (std::is_same_v<T, Unary>) {
          int val = eval(operations, cache, operation.src);
          if (operation.op == Operator::ASSIGN) {
            return val;
          } else if (operation.op == Operator::NOT) {
            return (~val) & 0xffff;
          }
        } else if constexpr (std::is_same_v<T, Binary>) {
          int a = eval(operations, cache, operation.src1);
          int b = std::holds_alternative<int>(operation.src2)
                      ? std::get<int>(operation.src2)
                      : eval(operations, cache,
                             std::get<std::string>(operation.src2));
          switch (operation.op) {
            case Operator::AND:
              return (a & b) & 0xffff;
            case Operator::OR:
              return (a | b) & 0xffff;
            case Operator::LSHIFT:
              return (a << b) & 0xffff;
            case Operator::RSHIFT:
              return (a >> b) & 0xffff;
            default:
              return 0;
          }
        }
        return 0;
      },
      op);

  cache[expr] = result;
  return result;
}

uint32_t process(std::string filename) {
  std::ifstream infile(filename);
  std::string line;
  std::map<std::string, Operation> operations;
  std::map<std::string, int> cache;
  std::array<std::pair<std::regex, MatchHandler>, 4> patterns = {{
      {std::regex(R"(^(\d+|\w+) -> (\w+)$)"),
       [&](const std::smatch& match) {
         std::string src = match[1].str();
         std::string dest = match[2].str();
         Operation op = Unary{Operator::ASSIGN, src};
         operations[dest] = op;
       }},
      {std::regex(R"(NOT (\d+|\w+) -> (\w+))"),
       [&](const std::smatch& match) {
         std::string src = match[1].str();
         std::string dest = match[2].str();
         Operation op = Unary{Operator::NOT, src};
         operations[dest] = op;
       }},
      {std::regex(R"((\d+|\w+) (AND|OR) (\d+|\w+) -> (\w+))"),
       [&](const std::smatch& match) {
         std::string src1 = match[1].str();
         Operator opr =
             (match[2].str() == "AND") ? Operator::AND : Operator::OR;
         std::string src2 = match[3].str();
         std::string dest = match[4].str();
         Operation op = Binary{opr, src1, src2};
         operations[dest] = op;
       }},
      {std::regex(R"((\d+|\w+) (LSHIFT|RSHIFT) (\d+) -> (\w+))"),
       [&](const std::smatch& match) {
         std::string src1 = match[1].str();
         Operator opr =
             (match[2].str() == "LSHIFT") ? Operator::LSHIFT : Operator::RSHIFT;
         int src2 = std::stoi(match[3].str());
         std::string dest = match[4].str();
         Operation op = Binary{opr, src1, src2};
         operations[dest] = op;
       }},
  }};
  while (std::getline(infile, line)) {
    for (const auto& [re, handler] : patterns) {
      std::smatch match;
      if (std::regex_match(line, match, re)) {
        handler(match);
      }
    }
  }
  int a = eval(operations, cache, "a");
  operations["b"] = Unary{Operator::ASSIGN, std::to_string(a)};
  cache = std::map<std::string, int>();
  return eval(operations, cache, "a");
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
