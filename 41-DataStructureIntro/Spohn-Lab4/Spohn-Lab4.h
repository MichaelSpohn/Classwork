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
		/*  2a - front and back private data members are declared. */
		Node* front;
		Node* back;
	public:
		SLL();
		~SLL();
		void insertToFront(int);
		void insertToEnd(int);
		void deleteLast();
		void deleteFront();
		void removeAllTargetNodes(int);
		void removeFirstTargetNode(int);
		void removeAllBUTFirstTargetNodes(int);
		bool unique();
};