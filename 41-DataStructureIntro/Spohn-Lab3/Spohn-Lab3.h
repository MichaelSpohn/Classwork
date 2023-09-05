#pragma once
using namespace std;

class Node {
	public:
		Node(int, Node*);
		int data;
		Node *next;
};

class SLL {
	private:
		/*  3a - front is declared as a node pointer. It's a private data member,
			so it can only be accessed by public functions in the SLL class.*/
		Node *front;
	public:
		SLL();
		~SLL();
		void insertToFront(int);
		void printAll();
		void deleteLast();
		void insertToMiddle(int);
		void deleteMiddle();
};