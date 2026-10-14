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

void selectionSort(int arr[], int n) {
  for(size_t i = 0; i < n; i++) {
    int min = arr[i];
    int minIndex = i;
    for(size_t j = i; j < n; j++) {
      if (arr[j] < min) {
        min = arr[j];
        minIndex = j;
      }
    }
    swap(&arr[i], &arr[minIndex]);
  } 
}

int main() {
    int arr[] = {12, 11, 13, 5, 6};
    int n = sizeof(arr) / sizeof(arr[0]);

    std::cout << "Unsorted array: " << std::endl;
    printArray(arr, n);

    selectionSort(arr, n);

    std::cout << "Sorted array: " << std::endl;
    printArray(arr, n);

    return 0;
}

