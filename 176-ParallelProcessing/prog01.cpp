// Michael Spohn
// CSCI 176
// Program Assignment 1

#include <iostream>
#include <unistd.h>   //for fork()
#include <sys/wait.h> //for wait()
#include <sys/time.h> //for macro GET_TIME(double)
#include <cstdlib>    //for exit(0)

using namespace std;

// macro from time-fork2.cpp
#define GET_TIME(now)\
{ struct timeval t; gettimeofday(&t, NULL);\
  now = t.tv_sec + t.tv_usec/1000000.0; }

// Fibonacci Recursive
double fib_rec(int n) {
    if (n <= 1) { return n; }
    return fib_rec(n-1) + fib_rec(n-2);
}

// Fibonacci Iterative
double fib_iter(int n) {
    double f1 = 1, f2 = 1, temp = 0;
    if (n == 1 || n == 2) { return 1; }
    else {
        for (int i = 3; i <= n; i++) {
            temp = f1 + f2;
            f1 = f2;
            f2 = temp;
        }
        return temp;
    }
}

// Main Function: Outputs the Fibonacci value based on an input. Uses fork() to
// utilize 3 child processes. Child 1 sends the values to the other child
// processes via 4 pipes. Child 2 calculates the result through recursion and
// child 3 calculates the result through iteration. The times of execution are
// calculated and sent back to the first child process to be outputted. Two
// for loops are used: one for the child processes and another for waiting for
// the processes to complete.
int main(int argc, char* argv[]) {
    int pid;
    int status;
    int p1[2], p2[2], p3[2], p4[2]; // 4 required pipes
    pipe(p1);                       // N from c1 to c2
    pipe(p2);                       // N from c1 to c3
    pipe(p3);                       // total from c2 to c1
    pipe(p4);                       // total from c3 to c1
    int N = atoi(argv[1]);          // Fibonacci integer for inputs
    int N2, N3;                     // Integers used for reading inputs
    cout << "N = " << N << endl;
    double start1, stop1, start2, stop2, total1, total2, t1, t2; // timing variables
    double result1, result2;        // used for outputting Fibonacci results in console

    // loop 3 child processes
    for (int i = 1; i <= 3; i++) {
        pid = fork();
        if (pid == 0 && i == 1) {               // c1
            cout << "---Child1" << endl;
            write(p1[1], &N, sizeof(int));      // write N to c2
            write(p2[1], &N, sizeof(int));      // write N to c3
            read(p3[0], &t1, sizeof(double));   // read total from c2
            read(p4[0], &t2, sizeof(double));   // read total from c3
            cout << "Recursive execution time: " << t1 << "s" << endl;
            cout << "Iterative execution time: " << t2 << "s" << endl;
            exit(0);
        }
        else if (pid == 0 && i == 2) {          // c2
            cout << "---Child2" << endl;
            read(p1[0], &N2, sizeof(int));      // read N from c1
            GET_TIME(start1);
            result1 = fib_rec(N2);
            GET_TIME(stop1);
            cout << "Child2 result: " << result1 << endl;
            total1 = stop1 - start1;            // calculate time
            write(p3[1], &total1, sizeof(double)); // write total to c1
            exit(0);
        }
        else if (pid == 0 && i == 3) {          // c3
            cout << "---Child3" << endl;
            read(p2[0], &N3, sizeof(int));      // read N from c1
            GET_TIME(start2);
            result2 = fib_iter(N3);
            GET_TIME(stop2);
            cout << "Child3 result: " <<  result2 << endl;
            total2 = stop2 - start2;            // calculate time
            write(p4[1], &total2, sizeof(double)); // write total to c1
            exit(0);
        }
    }

    // wait for each child process to finish
    for (int i = 1; i <= 3; i++) {
        pid = wait(&status);
        cout << "Child process pid = " << pid << " exited, status = " << status << endl;
    }

    return 0;
}