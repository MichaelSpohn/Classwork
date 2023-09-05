#pragma once
using namespace std;

class UnorderedMaxPQ {
	private:
		vector<int> pq;
		int N;
	public:
		UnorderedMaxPQ(vector<int> a);
		~UnorderedMaxPQ();
		void insert(int v);
		int delMax();
		bool isEmpty();
		void print();
};

class OrderedMaxPQ {
	private:
		vector<int> pq;
		int N;
	public:
		OrderedMaxPQ(vector<int> a);
		~OrderedMaxPQ();
		void sort();
		void insert(int v);
		int delMax();
		bool isEmpty();
		void print();
};

class HeapMaxPQ {
	private:
		vector<int> pq;
		int N;
		void sink(int k);	// Sink and swim are helper functions and don't need to be accessed outside the class.
		void swim(int k);
	public:
		HeapMaxPQ(vector<int> a);
		~HeapMaxPQ();
		void insert(int v);
		int delMax();
		bool isEmpty();
		void print();
};