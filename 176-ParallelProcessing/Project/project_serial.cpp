// Michael Spohn
// CSCI 176
// Project: HeapSort
// Serial heapsort that takes in command line argument n for array size,
// randomly fills the array with values 0~n, and sorts it with heapsort.
// The max heap is built from the middle point of the array, and then the
// root is recursively swapped to the end of the heap and removed, signifying
// its sorted position. After this looped process, the array has been sorted
// sucessfully and relative information is outputted.
//
// $> g++ -o xxx project_serial.cpp
// $> ./xxx n
// (n is array size to be sorted)

#include <iostream>
#include <sys/time.h>
using namespace std;

// macro from time-fork2.cpp
#define GET_TIME(now)\
{ struct timeval t; gettimeofday(&t, NULL);\
  now = t.tv_sec + t.tv_usec/1000000.0; }

int n;                      // n array size
int* arr;                   // Array (dynamic)
double start, stop, total;  // Timing variables

// Function from Park-serial-qsort.cpp that is used in verifying that an array has
// been sorted correctly
int check_sorted (int list1[], int n)
{ for (int i=0; i<n-1; i++)
    if (list1[i] > list1[i+1])
      return 0; //not sorted
  return 1; //sorted
}

void heapify(int arr[], int size, int i) {
    int max = i;                    // largest index is root
    int l = 2*i + 1;                // left child
    int r = 2*i + 2;                // right child

    // If left or right child is larger, update max
    if (l < size && arr[l] > arr[max]) max = l;
    if (r < size && arr[r] > arr[max]) max = r;

    // If child was larger than root, swap and heapify on the new max
    if (max != i) {
        swap(arr[i], arr[max]);
        heapify(arr, size, max);
    }
}

int main(int argc, char *argv[]) {
    GET_TIME(start);
    n = atoi(argv[1]);      // Command line arg for array size
    arr = new int[n];       // Array size of n

    // Random values put in array ranging from 0~n
    for (int i = 0; i < n; i++) arr[i] = rand() % n+1;

    // Output unsorted array (if size <= 100)
    if (n <= 100) {
        cout << "Original Array:" << endl;
        for (int i = 0; i < n; i++) cout << arr[i] << " ";
        cout << endl;
    }

    // Build the max heap from the middle point towards index 0
    for (int i = n/2 - 1; i >= 0; i--) heapify(arr, n, i);

    for (int i = n-1; i > 0; i--) {
        swap(arr[0], arr[i]);   // Swap root to end of heap
        heapify(arr, i, 0);     // Heapify on current index
    }

    GET_TIME(stop);
    total = stop - start;

    if (n <= 100) {
        cout << endl << "Sorted Array:" << endl;
        for (int i = 0; i < n; i++) cout << arr[i] << " ";
        cout << endl;
    }

    // Check whether the array has been sorted and output as such
    if (check_sorted(arr, n) == 1) cout<<"**verified that array is sorted."<<endl;
    else cout<<"**array is not completely sorted - check program!"<<endl;
    cout << "Execution time with " << n << " sized array: " << total << "s";

    return 0;
}