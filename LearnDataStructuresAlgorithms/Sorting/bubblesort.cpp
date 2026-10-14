#include <iostream>
#include <random>

void foo(int test) {
    std::cout << test << std::endl;
}

template<typename T>
void swapIf(T* a, T* b) {
	if(*a > *b) {
		T c = *a;
		*a = *b;
		*b = c;
	}
}

template<typename T>
void bubbleSort(T* start, T* end) {
	for(T* start_current = start; start_current != end; start_current++) {
		T* end_inner = end;
		for(T* j = start_current; j != end_inner; j++) {
			swapIf(start_current, j);	
		}
		end_inner--;
	}
}

int main() {
	double lower_bound = 0;
    double upper_bound = 10;
    std::uniform_real_distribution<double> unif(lower_bound,upper_bound);
    std::default_random_engine re;

	size_t s = 6;
	double* p = new double[s];
	for(size_t i = 0; i < s; i++) p[i] = unif(re);

	for(size_t i = 0; i < s; i++) std::cout << p[i] << "\t";
	std::cout << std::endl;
	bubbleSort(p, p + s);
	for(size_t i = 0; i < s; i++) std::cout << p[i] << "\t";
	std::cout << std::endl;
	delete[] p;
}
