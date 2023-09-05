#include <iostream>
#include <ctime>
using namespace std;

// This function is used for recording the execution times for the three algorithms. Uses ctime.
class Stopwatch {
private:
	time_t start, end;
public:
	void startClock() { time(&start); }
	void endClock() { time(&end); }
	double timeTaken() { return difftime(end, start); }
};

// This function is used to print out the content of an array. I used this for testing purposes, so 16 integers is the limit.
void printArr(int a[]) {
	for (int i = 0; i < 16; i++) { cout << a[i] << " "; }
}

// This function is used in the conquer phase of merge sort, which puts arrays back together and sorted
void merge(int a[], int aux[], int lo, int mid, int hi) {				// merge uses an array, a backup array, the beginning of an array, the middle of an array, and the end of an array
	for (int k = lo; k <= hi; k++) { aux[k] = a[k]; }					// for loop to add the contents of the array to the backup array (to avoid accidental overrides)
	int i = lo;															// i will start at the beginning of the array
	int j = mid + 1;													// j will start 1 position past the middle
																		// if there are only two values, i will be on the first and j will be on the second, since lo = mid
	for (int k = lo; k <= hi; k++) {									// for loop to do the swapping with variable k, which starts at the beginning of the array
		if (i > mid) { a[k] = aux[j++]; }								// if i has passed the middle of the array, the value at k will be the value at j, and j will increase by 1
		else if (j > hi) { a[k] = aux[i++]; }							// if j has passed the end of the array, the value at k will be the value at i, and i will increase by 1
		else if (aux[j] < aux[i]) { a[k] = aux[j++]; }					// if the value of j in the backup array is smaller than the value of i in the backup array, then the value at k will be the value at j, and j will increase by 1
		else { a[k] = aux[i++]; }										// if the value of i in the backup array is larger than the value of j (or anything else), then the value at k will be the value at i, and i will increase by 1
	}																	// the swapping will keep happening until k has gone passed the end of the array
}

// This function is used to divide and conquer an array. It mostly divides arrays up to be sorted, then calls to be merged
void mergesort(int a[], int aux[], int lo, int hi) {					// mergesort uses an array, a backup array, the beginning of an array, and the end of an array
	if (hi <= lo) { return; }											// if hi <= low (such as if there was only 1 value in an array), the function would return (this is the base case)
	int mid = (lo + hi) / 2;											// the middle of the array is determined with this formula and will constantly change with each subarray
	mergesort(a, aux, lo, mid);											// once a middle is set, this function will be called recursively to divide the left side of the original array
	mergesort(a, aux, mid + 1, hi);										// once the left side is divided, the right side will be divided as well
	merge(a, aux, lo, mid, hi);											// this function call will sort the subarrays and put them back together
}

// This function is used to partition and swap values of an array.
int partition(int a[], int lo, int hi) {								// partition uses an array, the beginning of an array, and the end of an array
	int i = lo;															// i will start at the beginning, which will soon move one position in front of it. lo is the pivot
	int j = hi + 1;														// j will start one position after the end (outside of the array) since it will soon move one position back to hi
	int temp;															// temp is used in the swapping process
	while (true) {														// infinite while loop - only false or breaks can end this
		while (a[++i] < a[lo]) { if (i == hi) break; }					// i is moved forward as long as the value at i is less than the pivot (lo). If at any chance i reaches the end of the array, the loop ends
		while (a[lo] < a[--j]) { if (j == lo) break; }					// j is moved backward as long as the value of the pivot is less than the value at j. If at any chance j reaches the beginning of the array, the loop ends
		if (i >= j) break;												// if i and j pass each other, the loop ends

		temp = a[i];													// it's time to swap values since the while loop hasn't ended
		a[i] = a[j];													// temp is used to hold the value at i as i gets j's value
		a[j] = temp;													// j gets what i's value was from temp
	}																	// if i hasn't reached the end, j hasn't reached the beginning, and the two pointers haven't passed each other, the loop continues
	temp = a[lo];														// once the while loop ends, temp will hold the value at the beginning of the array
	a[lo] = a[j];														// the new value at lo will be the value at j
	a[j] = temp;														// the value at j will be what the value at lo was
	return j;															// j is the new pivot, and is returned to the quicksort function
}																		// the array has been partitioned

// This function is used to constantly partition and swap values through recursion
void quicksort(int a[], int lo, int hi) {								// quicksort uses an array, the beginning of an array, and the end of an array
	if (hi <= lo) { return; }											// this is the base case, where not more partitioning can be done as there may only be one value left in the array
	int j = partition(a, lo, hi);										// j will the new pivot or the new hi point of a paritioned array
	quicksort(a, lo, j - 1);											// recursive call on the left side of the pivot
	quicksort(a, j + 1, hi);											// recursive call on the right side of the pivot
}																		// the function ends when all quicksorting is done, and each array has been returned

// main was used for testing and has stopwatch calls set up
int main() {
	int a[] = { 8, 3, 5, 2, 9, 10, 22, 25, 1, 15, 7, 2, 1, 3, 4, 9 };
	int b[] = { 8, 3, 5, 2, 9, 10, 22, 25, 1, 15, 7, 2, 1, 3, 4, 9 };
	int aux[16] = {};

	Stopwatch time;
	time.startClock();

	mergesort(a, aux, 0, 15);

	time.endClock();
	cout << "Time taken for mergesort: " << time.timeTaken() << " second(s)" << endl;

	printArr(a);
	cout << endl;

	time.startClock();

	quicksort(b, 0, 15);

	time.endClock();
	cout << "Time taken for quicksort: " << time.timeTaken() << " seconds(s)" << endl;

	printArr(b);

	system("pause");
	return 0;
}