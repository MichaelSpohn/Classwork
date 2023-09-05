// Michael Spohn
// CSCI 176
// Project: HeapSort
// OpenMP heapsort that takes in command line arguments p and n for thread count
// and array size, respectively. This program works the same as the serial program,
// however the first stage of the program, the initial heap-building, is done
// using OpenMP and conditional statements. The idea is to build the heap in
// parallel parts using leveling functions, which determine the current level an
// item is in the heap/array. After the heap is built, the rest of the program is
// the same as the serial program. The array is checked that it was sorted and
// relative information is outputted.
//
// $> g++ -fopenmp -o xxx project_heap.cpp
// $> ./xxx p n
// (p = thread_count, n = array size)

#include <cstdlib> // For atoi(), exit(0)
#include <iostream>
#include <omp.h>
using namespace std;

int thread_count, n, height, mid, r;
int* arr;
double start, stop, total;  // Timing variables

// Function from Park-serial-qsort.cpp that is used in verifying that an array has
// been sorted correctly
int check_sorted (int list1[], int n)
{ for (int i=0; i<n-1; i++)
    if (list1[i] > list1[i+1])
      return 0; //not sorted
  return 1; //sorted
}

int level(int size, int i, int h) {
    int l = 2*i + 1;                // left child
    while (l < size) {
        h++;
        l *= 2 + 1;
    }
    return h;
}

int revel(int size, int l, int h) {
    int i = (l-1) / 2;
    while (i > 1) {
        h++;
        i = (i-1) / 2;
    }
    return h;
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
    start = omp_get_wtime();        // Start timer
    thread_count = atoi(argv[1]);   // Command line arg1 for thread count
    n = atoi(argv[2]);              // Command line arg2 for list size
    arr = new int[n];               // Global array size of n
    mid = n/2-1;

    // Random values put in array ranging from 0~n
    for (int i = 0; i < n; i++) arr[i] = rand() % n+1;

    // Output unsorted array (if size <= 100)
    if (n <= 100) {
        cout << "Original Array:" << endl;
        for (int i = 0; i < n; i++) cout << arr[i] << " ";
        cout << endl;
    }

    int my_rank = omp_get_thread_num();     // Get thread number

    #pragma omp parallel num_threads(thread_count)
    {
            for (int i = mid; i >= 0; i--) {
                #pragma omp barrier
                r = revel(n, i, 1);
                if (r > 1 && my_rank < 2) {             // 2 threads max
                    heapify(arr, n, i);
                } else if (r > 2 && my_rank < 4) {      // 4 threads max
                    heapify(arr, n, i);
                } else if (r > 3 && my_rank < 8) {     // 8 threads max
                    heapify(arr, n, i);
                } else if (r > 4) {                    // 16 threads max
                    heapify(arr, n, i);
                } else if (my_rank == 0) {             // 1 thread only
                    heapify(arr, n, i);
                }
            }
    }
    
    for (int i = n-1; i > 0; i--) {
        swap(arr[0], arr[i]);   // Swap root to end of heap
        heapify(arr, i, 0);     // Heapify on current index
    }

    stop = omp_get_wtime();     // Stop timer
    total = stop - start;       // Calculate total time of sorting

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