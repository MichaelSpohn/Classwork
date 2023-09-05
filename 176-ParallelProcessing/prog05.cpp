// Michael Spohn
// CSCI 176
// Program Assignment 3
// Parallel mergesort using MPI and gnu qsort() for processes. One input is used to
// set the size of the array. Only one array is used in this program other than temp
// arrays, and the solution will be sent to process_0 after using qsort() on each
// local_list and merging them with Merge(). Outputting the local_list that is set
// differently in each process is also relying on process_0. Processes will send
// information back and forth to stay synced and to properly comput the sorted array.
//
// $> mpic++ -o xxx prog05.cpp
// $> mpiexec -n x ./xxx y (x = amount of threads/processes, y = array size)

#include <mpi.h>      //For MPI functions, etc 
#include <iostream>
#include <string>
#include <sstream>
using namespace std;

// Function from Park-serial-qsort.cpp that is used as the last parameter in qsort()
int Compare(const void* a_p, const void* b_p) {
  int a = *((int*)a_p);
  int b = *((int*)b_p);
  
  if (a < b) return -1;
  else if (a == b) return 0;
  else return 1; //(a > b)
}//Compare

// Merge function: Takes two lists and a size to merge them together (sorted) into the
// first list. Both lists have the same size, n. A temporary array is used that doubles
// the local_n size. When copying values over to the first array, there will be enough
// space since the lists aren't using the local_n size.
void Merge(int listA[], int listB[], int local_n) {
  int* temp = new int[2*local_n];
  int i = 0, j = 0, k = 0;

  while (i < local_n && j < local_n) {
    if (listA[i] < listB[j]) {
      temp[k] = listA[i];
      i++; k++;
    } else {
      temp[k] = listB[j];
      j++; k++;
    }
  }

  while (i < local_n) {
    temp[k] = listA[i];
    i++; k++;
  }
  while (j < local_n) {
    temp[k] = listB[j];
    j++; k++;
  }

  for (int i = 0; i < local_n*2; i++) listA[i] = temp[i];
  delete[] temp;
}

// Main function: Variables are the amount of processes/threads, current thread rank,
// sizes, and the array to be used throughout the program. MPI is initiated, assigning
// values for comm_sz and my_rank. Process_0 will determine the array size to be used
// and send that to the other processes. local_n is determined through n/comm_sz, and
// each process will have an array of size n filled with values up to local_n. Each
// local_list array is sorted through qsort() and merged together based on neighboring
// processes (0-1, 2-3, etc). Merge() will put the merged result into first array of
// the two partners, so in the end, Process_0 will have the final result through sends
// and receives. The result is then outputted, along with finalizing MPI.
int main(int argc, char *argv[]) {
  int comm_sz;                              // Number of processes/threads
  int my_rank;                              // My process rank
  int n, local_n;                           // Size variables
  int* local_list;                          // Array to be used with each process

  MPI_Init(NULL, NULL);                     // Initialize up MPI 
  MPI_Comm_size(MPI_COMM_WORLD, &comm_sz);  // Get num processes from -n x
  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);  // Get my_rank among all

  if (my_rank == 0) {                       // Process_0
    n = atoi(argv[1]);                      // Get array size from command line
    // Send size n to every other available process
    for (int p = 1; p < comm_sz; p++) MPI_Send(&n, 1, MPI_INT, p, 0, MPI_COMM_WORLD);
  } else {                                  // Not process_0
    // Receive size n from process_0
    MPI_Recv(&n, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  }

  local_n = n / comm_sz;                    // local_n size = total size / processes
  local_list = new int[n];                  // local_list array size is total size

  srand(my_rank+1);                         // Seed is process rank + 1
  // Initialize local_list with values up to local_n
  for (int i = 0; i < local_n; i++) local_list[i] = rand() % 100;

  // Output local_list from each process in process_0
  // If process_0, receive each local_list and output it with a temp array
  if (my_rank == 0) {
    cout << "Process_0: ";
    for (int i = 0; i < local_n; i++) cout << local_list[i] << " ";
    cout << endl;

    for (int p = 1; p < comm_sz; p++) {
      int* temp_list = new int[n];
      MPI_Recv(temp_list, n, MPI_INT, p, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
      cout << "Process_" << p << ": ";
      for (int i = 0; i < local_n; i++) cout << temp_list[i] << " ";
      cout << endl;
    }
  } else {  // If not process_0, send local_list to process_0
    MPI_Send(local_list, n, MPI_INT, 0, 0, MPI_COMM_WORLD);
  }

  // Sort each local_list
  qsort(local_list, local_n, sizeof(int), Compare);

  int divisor = 2;        // Used in determining even/odd local array
  int core_diff = 1;      // Used in determining partner
  bool done = false;      // If odd (or second partner), after send, will mark as true
  while (!done && divisor <= comm_sz) {
    if (my_rank % divisor == 0) {               // Even, partner 1 of 2, receiver
      int* temp_list = new int[n];              // Create temp array of size n
      int partner = my_rank + core_diff;        // Determine partner
      // Receive partner's local_list and set to temp_list
      MPI_Recv(temp_list, n, MPI_INT, partner, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
      Merge(local_list, temp_list, local_n);    // Merge the local_lists into this one
      local_n *= 2;                             // Double the local_n size for future merges
      delete[] temp_list;                       // Delete temp array
    } else {                                    // Odd, partner 2 of 2, sender
      int partner = my_rank - core_diff;        // Determine partner
      // Send local_list to partner
      MPI_Send(local_list, n, MPI_INT, partner, 0, MPI_COMM_WORLD);
      done = true;                              // Set to done; nothing else to do
    }
    divisor *= 2;                               // Double divisor and core difference
    core_diff *= 2;
  }                                             // After while, should be sorted in process_0

  // Display the sorted list in process_0 (can loop to local_n or n, should be same now)
  if (my_rank == 0) {
    cout << endl << "Sorted List: " << endl;
    for (int i = 0; i < local_n; i++) cout << local_list[i] << " ";
    cout << endl;
  }

  delete[] local_list;                          // Delete local_list
  MPI_Finalize();                               // Finalize MPI, all processes have finished
  return 0;
}