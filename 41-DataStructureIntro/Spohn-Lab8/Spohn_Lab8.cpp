#include <iostream>
#include <vector>
#include "Spohn_Lab8.h"
using namespace std;

// Unordered Max Priority Queue Functions ---------------------
UnorderedMaxPQ::UnorderedMaxPQ(vector<int> a) {				// Constructor: uses a vector to set up priority queue
	pq = a;													// Priority queue becomes the vector
	N = pq.size();											// N is the size of pq
}

UnorderedMaxPQ::~UnorderedMaxPQ() {							// Destructor: delete everything
	for (int i = 0; i < N; i++) { pq.pop_back(); }			// Loop through the pq and delete each value
	N = 0;													// The size goes back to 0
}

void UnorderedMaxPQ::insert(int v) {						// Insertion with a value
	N++;													// The size of the pq will increase by one
	pq.push_back(v);										// The value is pushed to the back of the pq
}

int UnorderedMaxPQ::delMax() {								// The maximum value of the pq will be deleted
	int max = pq[0];										// Values max and j are used, where max will hold the value and j will hold the position in the pq
	int j = 0;
	int temp;												// temp is used during the swapping process after the traversal through the pq
	for (int i = 0; i < N; ++i) {							// Loop through the pq to find any other large value than the first value in the pq
		if (max < pq[i]) { max = pq[i]; j = i; }			// If a larger value is found, max will become that value and j will become the position in the pq
	}
	temp = pq[j];											// Swap positions j and the end of the pq using temp
	pq[j] = pq[N-1];
	pq[N-1] = temp;
	
	pq.pop_back();											// Delete the last value of the pq, which is the max
	N--;													// The size decreases by 1
	return max;												// The maximum value that was removed from the pq is returned
}

bool UnorderedMaxPQ::isEmpty() { return N == 0; }			// Simple function to return whether or not the pq is empty. If it is, then N = 0. If it isn't, then N is not 0

void UnorderedMaxPQ::print(){								// Extra function that prints the value of the pq with a for loop
	for (int i = 0; i < N; ++i) { cout << pq[i] << " "; }
	cout << endl;
}
// ------------------------------------------------------------

// Ordered Max Priority Queue ---------------------------------
OrderedMaxPQ::OrderedMaxPQ(vector<int> a) {					// Constructor similar to UnorderedMaxPQ's constructor, however it inclues a call to the sort function
	pq = a;
	N = pq.size();
	sort();													// The pq needs to be sorted, as this is the Ordered Max Priority Queue class
}

OrderedMaxPQ::~OrderedMaxPQ() {								// Destructor similar to UnorderedMaxPQ's destructor
	for (int i = 0; i < N; i++) { pq.pop_back(); }
	N = 0;
}

void OrderedMaxPQ::sort() {									// Sorts the pq using the method of insertion sort
	for (int i = 0; i < N; i++) {							// Traversal through the pq with pointer i
		for (int j = i; j > 0; j--) {						// Traversal through the pq with pointer j
			if (pq[j] < pq[j - 1]) {						// j will be on or behind i and will check if the previous value is less than or greater than
				int temp = pq[j];							// If the value at j is actually less than the value prior to it, then they will be immediately swapped
				pq[j] = pq[j - 1];
				pq[j - 1] = temp;
			}												// This will keep happening until a value at j is greater than it's prior value, making it so that all values before i are sorted
			else { break; }									// If there's no more sorting to do, the for loop will break so that i can continue
		}
	}
}

void OrderedMaxPQ::insert(int v) {							// Insert function similar to UnorderedMaxPQ's, however it includes a call to the sort function
	N++;
	pq.push_back(v);
	sort();													// Since a new value is being inserted to the end of the pq, the pq needs to be sorted again
}

int OrderedMaxPQ::delMax() {								// The maximum value of the pq will be deleted, and this is a lot more simple due to the pq being in order
	int max = pq[N-1];										// Since the pq is ordered, the max will ALWAYS be at the end
	pq.pop_back();											// max is set to the highest value and then it's deleted from the pq
	N--;													// The size of the pq decreases by one
	return max;												// The maximum value that was removed will be returned
}

bool OrderedMaxPQ::isEmpty() { return N == 0; }				// Checks if the pq is empty just like UnorderedMaxPQ's

void OrderedMaxPQ::print() {								// Same helpful printing pq function just like in UnorderedMaxPQ's class
	for (int i = 0; i < N; i++) { cout << pq[i] << " "; }
	cout << endl;
}
// ------------------------------------------------------------


// Heap-based Max Priority Queue ------------------------------
HeapMaxPQ::HeapMaxPQ(vector<int> a) {						// Constructor: Sets the pq up for heap as well as the size, then organizes it
	pq.push_back(-999);										// Index 0 will be an out of the way value as index 1 will be the starting point in the heap
	for (int i = 0; i < a.size(); i++) { pq.push_back(a[i]); }	// Since index 1 is the starting point, a for loop will be used to take the vector's values and put them in the pq
	N = pq.size();											// N is set to the size of the pq, including the ignored index 0
	for (int i = 1; i < N; i++) {							// This loop is used to swim values up through the heap if necessary, as the pq needs to be organized in such a way
		swim(i);
	}
}

HeapMaxPQ::~HeapMaxPQ() {									// Destructor that works the same way as in the other two classes
	for (int i = 0; i < N; i++) { pq.pop_back(); }
	N = 0;
}

void HeapMaxPQ::sink(int k) {								// Sink will take an index from the pq and push it down through the heap if applicable
	while (2 * k <= N) {									// 2k is the children of a root, so as long as it remains under the size of the pq, the loop will run
		int j = 2 * k;										// j will be set to the value of 2k and increase over time (for swapping purposes)
		if (j < N - 1 && (pq[j] < pq[j+1])) { j++; }		// if the value at j is less than the value in front of it, then j will go forward in the pq
		if (pq[k] >= pq[j]) { break; }						// if the value at k is larger or equal to the value at j, then the loop will end as there's no need to sink (anymore)
		int temp = pq[k];									// Swapping of values at k and j, then k becomes what j was for the next iteration of the while loop
		pq[k] = pq[j];
		pq[j] = temp;
		k = j;
	}
}															// In the end, the point in the pq will sink to its correct position

void HeapMaxPQ::swim(int k) {								// Swim will take an index from the pq and pull it up through the heap if applicable
	while ((k > 1) && (pq[k / 2] < pq[k])) {				// k/2 is the root of children, so if the root is smaller than the stated index, then it definitely needs to swim up
		int temp = pq[k];									// Swapping process between root and child
		pq[k] = pq[k / 2];
		pq[k / 2] = temp;
		k = k / 2;											// k becomes the root for the next iteration
	}
}															// In the end, the point in the pq will swim to its correct position

void HeapMaxPQ::insert(int v) {								// Inserts a new value to the pq just like the previous classes, but it has to be ordered with swim
	pq.push_back(v);
	N++;
	swim(N);												// Since the new values is inserted at the end of the pq, it could possibly swim up
}

int HeapMaxPQ::delMax() {									// Deletes the maximum value of a pq, which should be at the top of the heap (index 1)
	int max = pq[1];										// max is set to the top of the heap
	int temp = 1;											// temp will be used in the swapping, and uses index 1 since that's the top of the heap
	pq[1] = pq[N - 1];
	pq[N - 1] = temp;
	N--;													// After the swap, the size of the pq decreases by 1
	pq.pop_back();											// The max is at the end, since the end value was swapped with the top value. The max is deleted
	sink(1);												// Now that the smallest value is at the top of the heap, it needs to sink back to the bottom
	return max;												// The maximum value is returned
}

bool HeapMaxPQ::isEmpty() { return N == 0; }				// Same function as in the previous classes

void HeapMaxPQ::print() {									// Same function as in the previous classes
	for (int i = 1; i < N; i++) { cout << pq[i] << " "; }
	cout << endl;
}
// ------------------------------------------------------------

int main() {
	// Test with Unordered Max Priority Queue for sending vector, deleting max, and printing
	vector<int> vect(5);
	for (int i = 0; i < vect.size(); ++i) { cin >> vect[i]; }
	UnorderedMaxPQ* pq1 = new UnorderedMaxPQ(vect);
	cout << "Deleted maximum item: " << pq1->delMax() << endl;
	pq1->print();

	// Test with Unodered Max Priority Queue for inserting value, deleting max, and printing
	int val;
	cin >> val;
	pq1->insert(val);
	cout << "Deleted maximum item: " << pq1->delMax() << endl;
	pq1->print();

	// Test with Ordered Max Priority Queue for sending vector, deleting max, and printing
	cout << endl << "Ordered: " << endl;
	OrderedMaxPQ* pq2 = new OrderedMaxPQ(vect);
	cout << "Deleted maximum item: " << pq2->delMax() << endl;
	pq2->print();

	// Test with Ordered Max Priority Queue for inserting value, deleting max, and printing
	pq2->insert(val);
	cout << "Deleted maximum item: " << pq2->delMax() << endl;
	pq2->print();
	
	// Test with Heap Max Priority Queue for sending vector, forming a heap/binary treee, deleting max, and printing
	cout << endl << "Heap: " << endl;
	HeapMaxPQ* pq3 = new HeapMaxPQ(vect);
	pq3->print();
	cout << "Deleted maximum item: " << pq3->delMax() << endl;
	pq3->print();
}