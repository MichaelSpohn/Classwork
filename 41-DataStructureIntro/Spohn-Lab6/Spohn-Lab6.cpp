#include <iostream>
#include <vector>
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

// This function is used to print a vector's contents. I used this to verify that the sorts were working.
void printVector(vector<int> vect) {
	for (int i = 0; i < vect.size(); i++) { cout << vect[i] << " "; }
}

// This function is constantly looking for the smallest value and swapping after each cycle through the vector.
void selectionSort(vector<int>& a) {
	Stopwatch time;
	time.startClock();

	int N = a.size();							// N is set to the size of the vector
	for (int i = 0; i < N; i++) {				// for loop with i, which increases by 1 after each swap
		int min = i;							// min is used for finding the smallest value
		for (int j = i + 1; j < N; j++) {		// for loop with j that goes through the vector and looks for the smallest value
			if (a[j] < a[min]) { min = j; }		// if a smaller value is found, min will be set to that position in the vector
		}
		int temp = a[i];						// at the end of the search through the vector, the swapping occurs with a temp integer
		a[i] = a[min];							// the value at i's position will be swapped with the value of min's position
		a[min] = temp;							// the value at min's position will now have i's value
	}

	time.endClock();							// with a vector (3000, 2999, 2998, ..., 1) the time was about 4 seconds (2nd best)
	cout << "Time taken for selection sort: " << time.timeTaken() << " second(s)" << endl;
}

// This function swaps two values in a vector whenever a smaller value is found
void insertionSort(vector<int>& a) {
	Stopwatch time;
	time.startClock();

	int N = a.size();							// N is set to the size of the vector
	for (int i = 0; i < N; i++) {				// for loop with i, which increases by 1 after all necessary swaps are done
		for (int j = i; j > 0; j--) {			// for loop with j, which decreases to 0 and moves smaller values to the front of the vector
			if (a[j] < a[j - 1]) {				// j-1 is used to see if the value j is at is smaller or not. If it is, j and the value before it will be swapped
				int temp = a[j];				// the swapping is done with a temp integer set to j's value in the vector
				a[j] = a[j - 1];				// the value at j's position is set to the value behind j's position
				a[j - 1] = temp;				// the value behind j's position will now have j's value, which is smaller
			}
			else { break; }						// if the value at j is not smaller, then there's no need to search to the beginning of the vector
		}
	}

	time.endClock();							// with a vector (3000, 2999, 2998, ..., 1) the time was about 12 seconds (the worst of the 3 sorts)
	cout << "Time taken for insertion sort: " << time.timeTaken() << " second(s)" << endl;
}

// This function is similar to insertion sort, but swaps two values in large gaps that decrease over time
void shellSort(vector<int>& a) {
	Stopwatch time;
	time.startClock();

	int N = a.size();							// N is set to the size of the vector
	int h = 7;									// h is the size of the gap between i and j, which could've been different from 7
	while (h >= 1) {							// the swapping will continue until h is 0 (or below 1, technically)
		for (int i = h; i < N; i++) {			// for loop with i, which increases by 1 after all necessary swaps are done
			for (int j = i; j >= h; j -= h) {	// for loop with j, which decreases to below the value of h and by h for the gap between i and j
				if (a[j] < a[j - h]) {			// if the value at j is less than the value h positions behind j, then j's will be swapped with that value
					int temp = a[j];			// temp value is set to the value at j
					a[j] = a[j - h];			// the value at j is set to the value h positions behind j
					a[j - h] = temp;			// the value h positions behind j is then set with the smaller value
				}
				else { break; }					// if the value at j isn't smaller than the value at the position h spots back, then there's no necessary swapping (at this time)
			}
		}
		h /= 2;									// after all swappings are done with this gap size, the gap size will divide by 2 (7/2 = 3, 3/2 = 1, 1/2 = 0, done)
	}

	time.endClock();							// with a vector (3000, 2999, 2998, ..., 1) the time was about 2 seconds (the best of the 3 sorts)
	cout << "Time taken for shell sort: " << time.timeTaken() << " second(s)" << endl;
}

int main() {
	/*  I used 3 vectors that were in descending order from 3000 to 1 to test the timing of
		each sort. I also made my own small vectors and debugged to verify that each sort worked
		as they should. I used the printVector when testing and removed those function calls afterward.
		Each sort should work as intended, I just wanted to leave these lines of code to show
		how I measured the timing. I wanted to do a lengthy sort to get noticiable differences
		in the times. */
	vector<int> vect1(3000);
	vector<int> vect2(3000);
	vector<int> vect3(3000);
	for (int i = 0; i < vect1.size(); i++) { vect1[i] = 3000 - i; }
	for (int i = 0; i < vect2.size(); i++) { vect2[i] = 3000 - i; }
	for (int i = 0; i < vect3.size(); i++) { vect3[i] = 3000 - i; }
	selectionSort(vect1);	// 2nd
	insertionSort(vect2);	// 3rd
	shellSort(vect3);		// 1st
	system("pause");
	return 0;
}