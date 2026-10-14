#include "header.hpp"
#include <strstream>

class person_t {
public:
  enum output_format { name_only, full_name };

  std::string name;
  std::string surname;
  person_t(std::string name_, std ::string surname_)
      : name(name_), surname(surname_) {}
};

void print_person(const person_t &person, std::ostream &out,
                  person_t::output_format format) {
  if (format == person_t::name_only) {
    out << person.name << '\n';

  } else if (format == person_t::full_name) {
    out << person.name << ' ' << person.surname << '\n';
  }
}

auto print_person_cd(const person_t &person) {
  return [&](std::ostream &out) {
    return [&](person_t::output_format format) {
      print_person(person, out, format);
    };
  };
}

int main(int argc, char *argv[]) {
  person_t Martha("Martha", "Whatever");
  print_person_cd(Martha)(std::cout)(person_t::full_name);
  return 0;
}
