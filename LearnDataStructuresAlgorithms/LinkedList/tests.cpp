#include "linkedlists.hpp"

void ass(const bool expr, const std::string& message) {
  if (!expr) {
    throw message;
  }
}

int main() {
  srand(time(nullptr));
  int i = 1;
  linkedList<int> list(i);
  ass(list.sz() == 1, "size has to be 1");
  ass(list[0] == 1, "first element is 1");

  for(int i = 2; i < 6; i++) {
    list.append(i);
  }
  for(size_t i = 0; i < 20; i++) {
    int idx = rand() % list.sz();
    int elem = rand() % 1000;
    list.insert(elem, idx);
    ass(list[idx] == elem, "element is correct");
  }

  bool catched = false;
  try {
    list[-1];
  }
  catch (const std::exception&e) {
    const std::string expected = "Index above size";
    ass(expected == e.what(), "Invalid index -1");
    catched = true;
  }
  ass(catched, "exception thrown");

  catched = false;
  try {
    list[100000];
  }
  catch (const std::exception&e) {
    const std::string expected = "Index above size";
    ass(expected == e.what(), "Invalid index 100000");
    catched = true;
  }
  ass(catched, "exception thrown");

  list[0] = 1;
  ass(list[0] == 1, "first element is 1");
  list[0] = 3;
  ass(list[0] == 3, "first element is now 3");
  list[2] = 30;
  ass(list[2] == 30, "third element is now 30");
  list[3] = 300;
  ass(list[3] == 300, "third element is now 300");
  list[4] = 3000;
  ass(list[4] == 3000, "fourth element is now 3000");
  list[5] = 3;
  ass(list[5] == 3, "sixth element is now 3");
}
