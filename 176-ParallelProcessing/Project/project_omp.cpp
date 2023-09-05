// Michael Spohn
// CSCI 176
// Project: HeapSort
// OpenMP heapsort that takes in command line arguments p and n for thread count
// and array size, respectively. This program works the same as the serial program,
// however the array is divided into parts and heapified/sorted back together
// until the array is fully sorted. This is done with OpenMP and the idea of
// merging parts of a divided array. After OpenMP is finished, the array is
// checked to see if the array is sorted and relative information is outputted.
//
// $> g++ -fopenmp -o xxx project_omp.cpp
// $> ./xxx p n
// (p = thread_count, n = array size)

#include <cstdlib> // For atoi(), exit(0)
#include <iostream>
#include <omp.h>
using namespace std;

int thread_count, n;
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

    // Random values put in array ranging from 0~n
    for (int i = 0; i < n; i++) arr[i] = rand() % n+1;

    // Output unsorted array (if size <= 100)
    if (n <= 100) {
        cout << "Original Array:" << endl;
        for (int i = 0; i < n; i++) cout << arr[i] << " ";
        cout << endl;
    }

    #pragma omp parallel num_threads(thread_count)
    {
        int local_n = n/thread_count;
        int my_rank = omp_get_thread_num();

        int divisor = 2;        // Used in determining even/odd local array
        while (divisor <= thread_count) {  
            #pragma omp barrier // Wait for all threads to finish
            if (my_rank % divisor == 0) {
                for (int i = local_n - 1; i >= 0; i--)
                    heapify(arr, local_n*2, i);
                for (int i = (local_n*2)-1; i > 0; i--) {
                    swap(arr[0], arr[i]);   // Swap root to end of heap
                    heapify(arr, i, 0);     // Heapify on current index
                }
                local_n*=2;
            }
            divisor *= 2;       // Double divisor
        }
    }

    stop = omp_get_wtime();
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