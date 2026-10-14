#include "header.hpp"
#include <ios>
#include <type_traits>

// remove reference meta function

// the base case
template <typename T> struct remove_reference {
  using type = T;
};

// specialise for a reference variable
template <typename T> struct remove_reference<T &> {
  using type = T;
};

// specialise for a r value
template <typename T> struct remove_reference<T &&> {
  using type = T;
};

template <typename T> void foo(T &&var) {
  std::cout << std::boolalpha
            << std::is_lvalue_reference_v<decltype(var)> << std::endl;
  std::cout << std::boolalpha
            << std::is_rvalue_reference_v<decltype(var)> << std::endl;
  std::cout << std::endl;
}

int main(int argc, char *argv[]) {
  int var = 1;
  int &ref = var;
  foo(var);
  foo(ref);
  foo(100);
  foo(var + 100);
  return 0;
}
