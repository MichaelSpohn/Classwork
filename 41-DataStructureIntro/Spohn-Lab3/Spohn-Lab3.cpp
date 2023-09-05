#include <iostream>
#include "Spohn-Lab3.h"
using namespace std;

/*  1 - The node class only has one function, and it's the node constructor. This function
	takes an integer to store for the data value of a node, and it takes a node pointer to
	store what comes after the node (by default it's NULL). Now any node that is declared
	will have a value and it will have a continuation, whether that's another node or NULL. */
Node::Node(int d = 0, Node* n = NULL) {
	data = d; next = n;
}

/*  3b - The SLL constructor takes the front node pointer and sets it to NULL, while the SLL
	destructor deletes front. */
SLL::SLL() { front = NULL; }	// front was declared as a private data member in Spohn-Lab3.h
SLL::~SLL() { delete front; }

/*  3c - This function works similarly to 2b, where a new node is declared and put at the front
	of a linked list. An integer value is passed to the function to act as the node's data value,
	and a new node is declared. The data is set to the integer, and the next value is set to front.
	It's possible that front could still be set to NULL if this is the first node in the linked
	list. Finally, front is then set to equal the new node. This is a void function, so nothing
	is returned. The front data member is modified by this function. */
void SLL::insertToFront(int x) {
	Node *nn = new Node();
	nn->data = x;
	nn->next = front;
	front = nn;
}

/*  3d - This function works similarly to 2d and is coded the same way. The purpose of this function
	is to loop/traverse through a linked list a print the data value of each node. This is done
	by declaring a new node and setting it to the front of the linked list. A while loop is used to
	traverse the linked list with the new node, outputting each one and then going to the next node
	for the next while loop. Once x reaches the end (NULL), the looping ends and all nodes have
	been printed. */
void SLL::printAll() {
	Node *x = front;
	while (x != NULL) {
		cout << x->data << endl;
		x = x->next;
	}
	cout << endl;
}

/*  3e - This function works similarly to 2e and is coded the same way. The purpose of this function
	is to loop/traverse through a linked list, find the last node in the list, and delete it. This is
	done by introducing two nodes, prev and curr, and setting them to the front of the linked list,
	which is the private data member, front. The while loop checks if the node after curr is NULL. If
	it isn't, prev is set to the node curr is on, and curr goes to the next node for the next loop.
	Once the next node is NULL, that means curr is on the last node of the list. The node after curr
	is deleted and prev is set as the new NULL. */
void SLL::deleteLast() {
	Node* prev = front;
	Node* curr = front;
	while (curr->next != NULL) {
		prev = curr;
		curr = curr->next;
	}
	delete curr->next;
	prev->next = NULL;
}

/*  3f - This function works similarly to 2c, where a new node is declared and inserted in the middle
	of an existing linked list. To do this, curr and prev are set to 0, and the size of the linked
	list is found by looping through it. The size is needed to find a middle point, which will be
	set to pos. With the position found, a for loop is used to traverse to the position where the
	new node will be inserted. Once there, the new node's data is set, the new node is put before curr,
	and the new node is put after prev. */
void SLL::insertToMiddle(int x) {
	Node *nn = new Node();
	Node *curr = front;
	Node *prev = front;
	int size = 0;
	int pos;

	while (curr != NULL) {
		curr = curr->next;
		size++;
	}
	pos = size / 2;

	curr = front;
	for (int i = 0; i <= pos; i++) {
		prev = curr;
		curr = curr->next;
	}
	nn->data = x;
	nn->next = curr;
	prev->next = nn;
}

/*  3g - This function works similarly to 2f but it's structered similarly to 3f. The goal
	is to remove the middle node, so by following the same steps as inserting a new node
	(finding the middle and traversing to that point with prev and curr), a node can be
	deleted. Once curr is at the middle node, prev is skipped over to the node after curr
	and curr is deleted. */
void SLL::deleteMiddle() {
	Node *curr = front;
	Node *prev = front;
	int size = 0;
	int pos;

	while (curr != NULL) {
		curr = curr->next;
		size++;
	}
	pos = size / 2;

	curr = front;
	for (int i = 0; i <= pos; i++) {
		prev = curr;
		curr = curr->next;
	}
	prev->next = curr->next;
	delete curr;
}

int main() {
	/*  2a - Nodes are declared and connected together. */
	Node *p, *q, *r, *s, *t;
	p = new Node(10);
	q = new Node(5, p);
	r = new Node(7, q);
	s = new Node(12, r);
	t = new Node(3, s);

	/*  2b - New node is declared and put before t, which is the current head of the
		linked list. */
	Node *u;
	u = new Node(8);
	u->next = t;

	/*  2c - New node is declared and put before r and after s, which is the middle of
		the linked list. */
	Node *v;
	v = new Node(9);
	v->next = r;
	s->next = v;

	/*  2d - New node is declared and set to the head of the linked list, which is u.
		A while loop is used to traverse the linked list and output the value of each
		node. As long as front doesn't equal NULL, which is at the end of the linked
		list, values will be outputted. Once front reaches the last node, p, front will
		equal NULL, ending the while loop. */
	Node *front;
	front = u;
	while (front != NULL) {
		cout << front->data << endl;
		front = front->next;
	}
	cout << endl;

	/*  2e - Two new nodes are declared and set to front, which is set to u (the head
		of the linked list. The while loop is used to traverse the linked list until
		the next node is NULL. This is done with the curr node. In every loop, prev
		is set to what curr was when the loop began, and curr goes to the next loop.
		Once the next node is NULL, the while loop ends and the next node after curr
		is deleted. Then, the node after prev is set as the new NULL node. */
	front = u;
	Node* prev = front;
	Node* curr = front;
	while (curr->next != NULL) {
		prev = curr;
		curr = curr->next;
	}
	delete curr->next;
	prev->next = NULL;

	/*  2f - New node is declared and is set to the v node, as that's in the middle
		of the linked list. Nodes front and curr are set to u, the head of the linked
		list, and a while loop is set for as long as front isn't at NULL. In every loop,
		curr will go to the node after front and see if the data of that node is the same
		as the data of the target. If it is, then front will go to the node after curr, curr
		will be deleted, and the loop ends immediately. If curr's data doesn't equal
		the target's, then front will go to the next node and the looping will begin again. */
	Node* target = v;
	front = u;
	curr = u;
	while (front != NULL) {
		curr = front->next;
		if (curr->data == target->data) {
			front->next = curr->next;
			delete curr;
			break;
		}
		else {
			front = front->next;
		}
	}

	system("pause");

	return 0;
}