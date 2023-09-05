// Michael Spohn
// CSCI 176
// Program Assignment 3
// Parallel mergesort using OpenMP and GNU qsort() for threads. Two inputs are
// used as command line arguments: p amount of threads and n size of the array
// to sort. The array is given random values ranging from 0 to n. OpenMP is then
// used to split the array based on the amount of threads are available into
// local arrays. These local arrays are sorted with qsort() and put back into
// the original array. The merge() function is called after determining the
// start indexes for two side-by-side local arrays to sort them and reapply
// them to the original array. After all local arrays have been sorted with their
// neighboring local arrays, the process is done again for further sorting,
// similar to the mergesort process. After this, OpenMP is complete and the
// array is verified to be sorted.
//
// $> g++ -fopenmp -o xxx prog04.cpp
// $> ./xxx p n     (p is amount of threads, n is array size)

#include <cstdlib> // For atoi(), exit(0)
#include <iostream>
#include <omp.h>
using namespace std;

int p, n;                   // p threads, n array size
int* arr;                   // global array (dynamic)
double start, stop, total;  // Timing variables

// Function from Park-serial-qsort.cpp that is used as the last parameter in qsort()
int Compare(const void* a_p, const void* b_p) 
{ int a = *((int*)a_p);
  int b = *((int*)b_p);
  
  if (a < b) return -1;
  else if (a == b) return 0;
  else return 1; //(a > b)
}//Compare

// Function from Park-serial-qsort.cpp that is used in verifying that an array has
// been sorted correctly
int check_sorted (int list1[], int n)
{ for (int i=0; i<n-1; i++)
    if (list1[i] > list1[i+1])
      return 0; //not sorted
  return 1; //sorted
}

// Merge function that takes the global array and the starting positions of two
// local/sub arrays. The size of the arrays is calculated and a temporary array
// based on that size is created. Values from the global array are compared based
// on the local array starting positions and sorted into the temporary array.
// Afterwards, the sorted temporary array replaces the values of the global
// array that have been compared. Any other part of the array is not utilized.
void merge(int arr[], int start1, int start2) {
    int size = start2 - start1;
    int* temp = new int[size*2];

    int i = 0, j = 0, k = 0;
    while (i < size && j < size) {
        if (arr[start1 + i] < arr[start2 + j]) {
            temp[k] = arr[start1 + i];
            i++; k++;
        } else {
            temp[k] = arr[start2 + j];
            j++; k++;
        }
    }

    while (i < size) {
        temp[k] = arr[start1 + i];
        i++; k++;
    }
    while (j < size) {
        temp[k] = arr[start2 + j];
        j++; k++;
    }
    
    #pragma omp parallel for
        for (i = 0; i < size*2; i++) arr[start1 + i] = temp[i];
}

// Main function
// Initializes the global array with random values based on the size of the array.
// Threads are created with OpenMP based on input, and local arrays are created.
// Local arrays have a size of (global array size) / (number of threads).
// Local arrays are sorted with qsort() and added back into the global array
// from where they were copied from. They are then completely sorted using the
// merge() function created above. Additional information is outputted if the
// array size is <= 100.
int main(int argc, char* argv[]) {
    p = atoi(argv[1]);   // Command line arg1 for thread count
    n = atoi(argv[2]);   // Command line arg2 for list size
    arr = new int[n];    // Global array size of n

    // Random values put in global array ranging from 0~n
    for (int i = 0; i < n; i++) arr[i] = rand() % n+1;

    // Output unsorted global array (if size <= 100)
    if (n <= 100) {
        cout << "Original Array:" << endl;
        for (int i = 0; i < n; i++) cout << arr[i] << " ";
        cout << endl;
    }

    start = omp_get_wtime();    // Start timer

    // OpenMP begins with p threads
    #pragma omp parallel num_threads(p)
    {
        int local_n = n/p;                      // Local array size is n/p
        int* local_arr = new int[local_n];      // Create local array
        int my_rank = omp_get_thread_num();     // Get thread number
        int start_index = my_rank * local_n;    // Calculate start of local array
        int end_index = start_index + local_n - 1;  // Calculate end of local array

        // Copy from global array to local array based on start_index
        #pragma omp parallel for
            for (int i = 0; i < local_n; i++) local_arr[i] = arr[start_index + i];

        // Output unsorted global array (if size <= 100)
        if (n <= 100) {
            #pragma omp parallel
            {
                #pragma omp critical
                {
                    cout << "Thread_" << my_rank << ", local_list: ";
                    for (int i = 0; i < local_n; i++) cout << local_arr[i] << " ";
                    cout << endl;
                }
            }
        }

        // Sort the local array with GNU qsort()
        qsort(local_arr, local_n, sizeof(int), Compare);

        // Output sorted local array (if size <= 100)
        if (n <= 100) {
            #pragma omp parallel
            {
                #pragma omp critical
                {   
                    cout << "Thread_" << my_rank << ", sorted local_list: ";
                    for (int i = 0; i < local_n; i++) cout << local_arr[i] << " ";
                    cout << endl;
                }
            }
        }
        
        // Copy back from local array to global array based on start_index
        #pragma omp parallel for
            for (int i = 0; i < local_n; i++) arr[start_index + i] = local_arr[i];

        #pragma omp barrier     // Wait for all threads to finish

        int divisor = 2;        // Used in determining even/odd local array
        int core_diff = 1;      // Used in determining partner
        int start1, start2;     // Starting positions for local arrays
        while (divisor <= p) {  // Loop until all local arrays sorted
            #pragma omp barrier // Wait for all threads to finish
            // If even thread, set starting positions for the local arrays that are
            // to be sorted/merged with merge().
            // Else, do nothing
            if (my_rank % divisor == 0) {
                start1 = my_rank * local_n;
                start2 = (my_rank + core_diff) * local_n;
                merge(arr, start1, start2);
            }
            divisor *= 2;       // Double divisor
            core_diff *= 2;     // Double core difference
        }
    } // End of OpenMP

    stop = omp_get_wtime();     // Stop timer
    total = stop - start;       // Calculate total time of sorting

    // Output sorted global array (if size <= 100)
    if (n <= 100) {
        cout << "Sorted Array:" << endl;
        for (int i = 0; i < n; i++) cout << arr[i] << " ";
        cout << endl;
    }

    // Check whether the array has been sorted and output as such
    if (check_sorted(arr, n) == 1) cout<<"**verified that list1 is sorted."<<endl;
    else cout<<"**list1 is not completely sorted - check program!"<<endl;

    // Output the amount of threads used, the size of the global array, and the
    // amount of time taken
    cout << "p=" << p << ", n=" << n << ", Execution Time: " << total << "s" << endl;
 
    return 0;
}