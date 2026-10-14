#ifndef LINKED_LIST
#define LINKED_LIST

#include <iostream>
#include <stdexcept>
#include <cstdlib>
#include <ctime>
#include <cassert>

template<typename T>
class Node {
public:
  T val;
  Node* next;
  Node* previous;

  Node(T& inp) : val(inp) {}
  Node(T&& inp) : val(inp) {}
};


template<typename T>
class linkedList {
private:
  Node<T>* head = nullptr;
  Node<T>* tail = nullptr;
  size_t size = 0;

public:
  linkedList(T& inp) {
    head = new Node<T>(inp);
    head -> previous = nullptr;
    head -> next = nullptr;
    tail = head;
    size = 1;
  }
  ~linkedList() {
    if (head -> next == nullptr) {
      delete head;
      return;
    }
    while(head -> next != nullptr) {
      auto toDelete = head;
      head = head -> next;
      delete toDelete;
    }
    delete head;
  }

  void append(T& elem) {
    Node<T>* newNode = new Node<T>(elem);
    newNode -> next = nullptr;
    newNode -> previous = tail;
    tail -> next = newNode;
    tail = newNode;
    size++;
  }

  void insert(T& elem, size_t idx) {
    Node<T>* newNode = new Node<T>(elem); 
    if(idx > size) throw std::range_error("Index above size");

    if(idx == size) {
      newNode -> next = nullptr;
      newNode -> previous = tail;
      tail -> next = newNode;
      tail = newNode;
      size++;
      return;
    } else if(idx == 0) {
      newNode -> previous = nullptr;
      newNode -> next = head;
      head -> previous = newNode;
      head = newNode;
    }
    else if(idx > size/2) {
      auto copyTail = tail;
      size_t counter = size - 1; 
      while(counter > idx) { 
        copyTail = copyTail -> previous;
        counter--;
      }
      newNode -> previous = copyTail -> previous;
      newNode -> next = copyTail;
      (copyTail -> previous) -> next = newNode;
      copyTail -> previous = newNode;
    }
    else { 
      size_t counter = 0;
      auto copyHead = head;
      while(counter < idx) {
        copyHead = copyHead -> next;
        counter++;
      }
      auto leftNode = copyHead -> previous;
      newNode -> previous = leftNode;
      newNode -> next = copyHead;
      leftNode -> next = newNode;
      copyHead -> previous = newNode;
    }
    size++;
  }

  T operator[](size_t idx) const {
    if(idx > size) throw std::range_error("Index above size");
    if(idx < 0) throw std::range_error("Index smaller 0");

    if(idx == size) {
      return tail -> val;
    } else if(idx == 0) {
      return head -> val;
    } else if(idx > size/2) {
      auto copyTail = tail;
      size_t counter = size - 1; 
      while(counter > idx) { 
        copyTail = copyTail -> previous;
        counter--;
      }
      return copyTail -> val;
    }
    size_t counter = 0;
    auto copyHead = head;
    while(counter < idx) {
      copyHead = copyHead -> next;
      counter++;
    }
    return copyHead -> val;
  }

  T& operator[](size_t idx) {
    if(idx > size) throw std::range_error("Index above size");
    if(idx == size) {
      return tail -> val;
    } else if(idx == 0) {
      return head -> val;
    } else if(idx > size/2) {
      auto copyTail = tail;
      size_t counter = size - 1; 
      while(counter > idx) { 
        copyTail = copyTail -> previous;
        counter--;
      }
      return copyTail -> val;
    }
    size_t counter = 0;
    auto copyHead = head;
    while(counter < idx) {
      copyHead = copyHead -> next;
      counter++;
    }
    return copyHead -> val;
  }

  void print() const noexcept {
    auto copyHead = head;
    int counter = 0;
    std::cout << "[";
    while(copyHead -> next != nullptr) {
      std::cout << copyHead -> val << ", ";
      copyHead = copyHead -> next;
      if(counter == 10) std::cout << std::endl;
      counter++;
    }
    std::cout << copyHead -> val << "]" << std::endl;
  }

  void printAddress() const noexcept {
    auto copyHead = head;
    size_t idx = 0;
    while(copyHead -> next != nullptr) {
      std::cout << "Idx: " << idx << "\t val: " << copyHead -> val << 
        "\t previous: " << copyHead -> previous <<
        "\t addr: " << copyHead <<
        "\t next: " << copyHead -> next << std::endl;
      idx++;
      copyHead = copyHead -> next;
    }
    std::cout << "Idx: " << idx << "\t val: " << copyHead -> val << 
      "\t previous: " << copyHead -> previous <<
      "\t addr: " << copyHead <<
      "\t next: " << copyHead -> next << std::endl;
  }

  void checkAddresses() const noexcept {
    Node<T>* p = nullptr;
    Node<T>* a = nullptr;
    Node<T>* n = nullptr;
    Node<T>* copyHead = head;
    assert(head -> previous == nullptr);
    assert( (head -> next) -> previous == head);
    while(copyHead -> next != nullptr) {
      copyHead = copyHead -> next;
      if(copyHead == tail) break;
      assert( (copyHead -> next) -> previous == copyHead);
      assert( (copyHead -> previous) -> next == copyHead);
    }
    assert(copyHead -> next == nullptr);
    assert( (copyHead -> previous) -> next == copyHead);
  }

  size_t sz() const noexcept {
    return size;
  }
};

#endif
