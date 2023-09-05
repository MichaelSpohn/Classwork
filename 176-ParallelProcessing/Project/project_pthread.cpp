// Michael Spohn
// CSCI 176
// Project: HeapSort
// PThread heapsort that takes in command line arguments p and n for thread count
// and array size, respectively. The program acts the same as the serial heapsort,
// however pthreads are used in the second stage of the program: the sorting stage.
// The issue with this implementation is that there is no parallel work and
// each thread has to wait their turn to swap and heapify within the for loop.
// After these two stages, the array will be checked for whether or not it has
// been sorted correctly and relative information is outputted.
//
// $> g++ -o xxx project_pthread.cpp -lpthread
// $> ./xxx p n
// (p = thread_count, n = array size)

#include <iostream>
#include <cstdlib>
#include <sys/time.h>
#include <pthread.h> 
using namespace std;

// macro from time-fork2.cpp
#define GET_TIME(now)\
{ struct timeval t; gettimeofday(&t, NULL);\
  now = t.tv_sec + t.tv_usec/1000000.0; }

int n;                      // n array size
int thread_count;           // Used in command line for thread amount
long j;
int* arr;                   // Array (dynamic)
double start, stop, total;  // Timing variables
pthread_mutex_t mutex1;     // Used for updating arr
pthread_mutex_t mutex2;     // Used for cout statements

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

void *Slave(void* rank);    // Prototype for thread/slave function

int main(int argc, char *argv[]) {
    GET_TIME(start);
    long thread_id;
    thread_count = atoi(argv[1]);
    n = atoi(argv[2]);
    arr = new int[n];       // Array size of n
    j = n-1;

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

    if (n <= 100) {
        cout << endl << "Heaped Array:" << endl;
        for (int i = 0; i < n; i++) cout << arr[i] << " ";
        cout << endl;
    }

    pthread_t myThreads[thread_count]; // Define threads
    pthread_mutex_init(&mutex1, NULL); // Initialize mutex1
    pthread_mutex_init(&mutex2, NULL); // Initialize mutex2

    // Creates threads based on thread_count
    for (thread_id = 0; thread_id < thread_count; thread_id++)
        pthread_create(&myThreads[thread_id], NULL, Slave, (void*)thread_id);

    // Waits until all threads have finished and joins back to main
    for (thread_id = 0; thread_id < thread_count; thread_id++)
        pthread_join(myThreads[thread_id], NULL);

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

void *Slave(void* rank) {
    int my_rank = (long) rank;

    for (; j > 0; j--) {
        pthread_mutex_lock(&mutex1);
        if (j <= 0) break;
        //cout << "Thread_" << my_rank << ": " << j << endl;
        if (arr[0] > arr[j]) {
            swap(arr[0], arr[j]);   // Swap root to end of heap
            heapify(arr, j, 0);     // Heapify on current index
        }
        pthread_mutex_unlock(&mutex1);
    }
    pthread_mutex_unlock(&mutex1);

    return NULL;
}