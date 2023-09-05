// Michael Spohn
// CSCI 176
// Program Assignment 2
// Parallel global sum computation using pthreads. The amount of threads to use
// is input as a command line argument. An array of 500,000,000 integers is
// created (1-500,000,000). This program will add up all the values of the array
// and output the result as well as the amount of time it took, along with
// thread information. The more threads being used, the faster the program will
// be up to a certain point. 8 might be the highest for best results, but 16
// is also a possibility.

#include <iostream>
#include <cstdlib>
#include <sys/time.h>
#include <pthread.h> 
using namespace std;

// macro from time-fork2.cpp
#define GET_TIME(now)\
{ struct timeval t; gettimeofday(&t, NULL);\
  now = t.tv_sec + t.tv_usec/1000000.0; }

// Global area
int *arr = new int[500000000];  // Array of size 500,000,000
int thread_count;               // Used in command line for thread amount
long int global_sum = 0;        // Variable to hold global result
double start, stop, total;      // Timing variables
pthread_mutex_t mutex1;         // Used for cout statements
pthread_mutex_t mutex2;         // Used for updating global sum

void *Slave(void* rank);        // Prototype for thread/slave function

// Main function
// Takes an input for thread count and creates threads to add up the entirety
// of the array's contents. After all threads have finished with their
// computations, output the global sum and exectution time.
int main(int argc, char* argv[]) {
    GET_TIME(start);            // Start timer
    long thread_id;             // Variable for thread ID
    thread_count = atoi(argv[1]); // Amount of threads being used via cmd line

    // For loop to initialize the array (arr[] = {1, 2, 3, ..., 500000000})
    for (long int i = 0; i < 500000000; i++) arr[i] = i + 1;

    pthread_t myThreads[thread_count]; // Define threads
    pthread_mutex_init(&mutex1, NULL); // Initialize mutex1
    pthread_mutex_init(&mutex2, NULL);  // Initialize mutex2

    // Creates threads based on thread_count
    for (thread_id = 0; thread_id < thread_count; thread_id++)
        pthread_create(&myThreads[thread_id], NULL, Slave, (void*)thread_id);

    // Waits until all threads have finished and joins back to main
    for (thread_id = 0; thread_id < thread_count; thread_id++)
        pthread_join(myThreads[thread_id], NULL);
    
    GET_TIME(stop);             // Stop timer
    total = stop - start;       // Calculate total time of calculation(s)
    
    cout << "Global Sum: " << global_sum << endl;
    cout << "Execution Time: " << total << "s" << endl;

    return 0;
}

// Slave function
// Divides the addition process among the amount of threads available and assigns
// each thread a range of the array to add to the patial sum variable. After
// a thread is done adding, it will output information related to the work
// completed and add the sum of thread to the global sum.
void *Slave(void* rank) {
    long int partial_sum = 0;           // Sum for the thread
    int my_rank = (long) rank;          // Cast to long for rank of thread

    long int p = 500000000 / thread_count; // Divide the array evenly by amount of threads
    long int start_index = my_rank * p; // Calculate start index by with rank and p
    long int end_index = start_index + p - 1; // Calculate end index with start and p

    // For loop to add all values in the array from start to end indexes
    for (long int i = start_index; i <= end_index; i++) partial_sum += arr[i];

    pthread_mutex_lock(&mutex2);        // Reserve mutex1 for couts
    cout << "  thread_id: " << my_rank << endl;
    cout << "start_index: " << start_index << endl;
    cout << "  end_index: " << end_index << endl;
    cout << "partial_sum: " << partial_sum << endl;
    cout << "------------------------------" << endl << endl;
    pthread_mutex_unlock(&mutex2);      // Release mutex1

    pthread_mutex_lock(&mutex1);        // Reserve mutex2 for updating global sum
    global_sum += partial_sum;          // Update global sum from result of thread
    pthread_mutex_unlock(&mutex1);      // Release mutex2
    
    return NULL;
}