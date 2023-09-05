// Michael Spohn
// CSCI 176
// Program Assignment 3
// Matrix multiplication using pthreads and the equation: A(L*m) * B(m*n) = C(L*n)
// Four inputs are used as command line arguments: L rows and m columns for A,
// m rows and n columns for B, and L rows and n columns for C as the result.
// Input is in the order of L, m, n, thread_count. The matrices are initialized
// with i+j+1 on A and i+j on B. The program will then perform matrix multiplication
// on the matrices via the use of threads. The output will show the first 10x10
// and last 10x10 of the C matrix.
// NOTE: Transpose method works! Uncomment line 122 and comment 121 to try out the
// transpose method, which uses matrix BT.

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
double** A;                     // Matrix A: Lxm
double** B;                     // Matrix B: mxn
double** C;                     // Matrix C: Lxn
double** BT;                    // Matrix BT: Transpose of B
int thread_count, L, m, n;      // Command line variables
double start, stop, total;      // Timing variables
pthread_mutex_t mutex1;         // Used for updating C
pthread_mutex_t mutex2;         // Used for cout statements

void *Slave(void* rank);        // Prototype for thread/slave function

// Main function
// Initializes the three 2D arrays/matrices, create threads based on input,
// and pass the information to the slave function in order to compute the values
// of C. After all threads have finished and C is complete, the first 10x10
// and last 10x10 of the matrix will be outputted along with execution time.
int main(int argc, char* argv[]) {
    GET_TIME(start);            // Start timer
    long thread_id;             // Variable for thread ID
    L = atoi(argv[1]);          // Rows of A and C
    m = atoi(argv[2]);          // Columns of A and rows of B
    n = atoi(argv[3]);          // Columns of B and C
    thread_count = atoi(argv[4]);   // Amount of threads being used via cmd line

    // Initialize A with i+j+1 values in 2D array
    A = new double*[L];
    for (int i = 0; i < L; i++) {
        A[i] = new double[m];
        for (int j = 0; j < m; j++) A[i][j] = i+j+1;
    }

    // Initialize B with i+j values in 2D array
    B = new double*[m];
    for (int i = 0; i < m; i++) {
        B[i] = new double[n];
        for (int j = 0; j < n; j++) B[i][j] = i+j;
    }

    // Initialize C
    C = new double*[L];
    for (int i = 0; i < L; i++) C[i] = new double[n];

    // Used for transposing B and using the alternative method of matrix multiplication
    BT = new double*[n];
    for (int i = 0; i < n; i++) {
        BT[i] = new double[m];
        for (int j = 0; j < m; j++) BT[i][j] = B[j][i];
    }

    cout << "L = " << L << ", m = " << m << ", n = " << n << endl;

    pthread_t myThreads[thread_count]; // Define threads
    pthread_mutex_init(&mutex1, NULL); // Initialize mutex1
    pthread_mutex_init(&mutex2, NULL); // Initialize mutex2

    // Creates threads based on thread_count
    for (thread_id = 0; thread_id < thread_count; thread_id++)
        pthread_create(&myThreads[thread_id], NULL, Slave, (void*)thread_id);

    // Waits until all threads have finished and joins back to main
    for (thread_id = 0; thread_id < thread_count; thread_id++)
        pthread_join(myThreads[thread_id], NULL);
    
    GET_TIME(stop);             // Stop timer
    total = stop - start;       // Calculate total time of calculation(s)

    // Output first 10x10 of C
    cout << "=== C: [first_10 * first_10] ===" << endl;
    for (int i = 0; i < 10; i++) {
        for (int j = 0; j < 10; j++) cout << C[i][j] << " ";
        cout << endl;
    }
    // Output last 10x10 of C
    cout << "=== C: [last_10 * last_10] ===" << endl;
    for (int i = L-10; i < L; i++) {
        for (int j = n-10; j < n; j++) cout << C[i][j] << " ";
        cout << endl;
    }
    cout << "Execution Time: " << total << "s" << endl;

    return 0;
}

// Slave function
// Divides the matrix multiplication process among the amount of threads available
// and assigns each thread rows to calculate for. After a thread is done with its
// assigned rows, it will output that it has finished and which rows it calculated on.
void *Slave(void* rank) {
    int my_rank = (long) rank;              // Cast to long for rank of thread
    long a;                                 // Temporary value for flushing
    for (int i = my_rank; i < L; i += thread_count) { // Hops based on thread count
        for (int j = 0; j < n; j++) {
            a = 0;                          // Reset temporary value to 0
            for (int k = 0; k < m; k++) {
                // Comment one of the two following lines:
                a += A[i][k] * B[k][j];     // Matrix multiplication
                //a += A[i][k] * BT[j][k];    // Transpose matrix multiplication
            }
            pthread_mutex_lock(&mutex1);    // Reserve mutex1 for updating C
            C[i][j] = a;                    // Set result of multiplication to C[i][j]
            pthread_mutex_unlock(&mutex1);  // Reserve mutex1 for updating C
        }        
    }
    pthread_mutex_lock(&mutex2);            // Reserve mutex2 for couts
    cout << "Thread_" << my_rank << ": " << my_rank << " ~ " << L-1 << ", step" << thread_count << endl;
    pthread_mutex_unlock(&mutex2);          // Reserve mutex1 for couts

    return NULL;
}