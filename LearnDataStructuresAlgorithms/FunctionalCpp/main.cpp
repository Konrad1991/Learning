#include "header.hpp"
#include <functional>
#include <iostream>

/*
Problem:
I have a tree representing a task which has to be done.
Now, the information in the type of the tree should be used to create a new
tree. The new tree contains the information of how to calculate the derivative
of the original tree.
1. Calculate new tree
2. bind old variable at new tree. The variable is not constexpr
Probably not possible
*/

struct PrintType {
  template <typename T> void operator()(const T &arg) const {
    std::cout << "Type: " << typeid(arg).name() << std::endl;
  }
};

int main(int argc, char *argv[]) {
  constexpr auto is_greater_42 =
      std::bind(std::greater<double>(), std::placeholders::_1, 42);

  std::cout << is_greater_42(1) << std::endl;
  int t = 1;
  constexpr auto test =
      std::bind([](auto &&printer, auto &&value) { printer(value); },
                PrintType(), std::placeholders::_1);

  test(t);

  return 0;
}
