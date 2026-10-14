#include <iostream>

void printArray(int arr[], int size) {
  for(size_t i = 0; i < size; i++) {
    std::cout << arr[i] << "\t";
  }
  std::cout << std::endl;
}

void swap(int* a, int* b) {
  int temp = *a;
  *a = *b;
  *b = temp;
}

void insertionSort(int arr[], int n) {
  int sorted = 1;
  for(size_t i = 1; i < n; i++) {
    sorted++;
    for(size_t j = 0; j < sorted; j++) {
      if (arr[i] < arr[j]) {
        swap(&arr[i], &arr[j]);
      }
    } 
  } 
}

int main() {
    int arr[] = {12, 11, 13, 5, 6};
    int n = sizeof(arr) / sizeof(arr[0]);

    std::cout << "Unsorted array: " << std::endl;
    printArray(arr, n);

    insertionSort(arr, n);

    std::cout << "Sorted array: " << std::endl;
    printArray(arr, n);

    return 0;
}

