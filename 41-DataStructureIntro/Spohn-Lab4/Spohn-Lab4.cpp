#include <iostream>
#include "Spohn-Lab4.h"
using namespace std;

/*  1 - Node class has one function, and it's the constructor to initialize data and next.
	An integer and node are passed to the function for data and next. If no node is passed,
	next will be NULL. */
Node::Node(int d = 0, Node* n = NULL) {
	data = d; next = n;
}

/*  2b - SLL constructor initializes front and back to NULL. SLL destructor uses a node and
	while loop to traverse the linked list and delete each node. Once the linked list has
	been deleted, front and back are deleted. (front and back were declared in Spohn-Lab4.h) */
SLL::SLL() { front = NULL; back = NULL; }
SLL::~SLL() {
	Node *x;
	while (front != NULL) {
		x = front;
		front = front->next;
		delete x;
	}
	delete front;	delete back;
}

/*  2c - This function inserts a new node at the front of a linked list by passing an integer for
	the node's data. A new node is created with the data as the passed integer and NULL as the
	next node. If the list is empty, then the front and the back will be the new node. If there's
	only one node in the list, then the new node will be put before the front node, the front will
	be set to the new node, and back will be set to the next node after front. If there are multiple
	nodes in the list, then the new node will simply be put before the current front, and front
	will be set to the new node. */
void SLL::insertToFront(int x) {
	Node *nn = new Node(x);
	if (front == NULL) {
		front = nn;
		back = front;
	}
	else if (front->next == NULL) {
		nn->next = front;
		front = nn;
		back = front->next;
	}
	else {
		nn->next = front;
		front = nn;
	}
}

/*  2d - This function inserts a new node at the end of a linked list by passing an integer for
	the node's data. A new node is created with the data as the passed integer and NULL as the
	next node. If the list is empty, then the front and back node will just be the new node. If
	there's only one node in the list, then front's next node will the new node and that will also
	be set as the back. If there are multiple nodes in the list, then the new node will be placed
	after back, and then back will be set to the new node. */
void SLL::insertToEnd(int x) {
	Node *nn = new Node(x);
	if (front == NULL) {
		front = nn;
		back = front;
	}
	else if (front->next == NULL) {
		front->next = nn;
		back = nn;
	}
	else {
		back->next = nn;
		back = nn;
	}
}

/*  2e - This function deletes the last node of a linked list. If the list is already empty, then
	front and back will be set to NULL, if they weren't already. If the list has only one node,
	front and back would be deleted and then both would be set to NULL. If the list has multiple
	nodes, then a new node is set at the front to traverse to the end of the linked list. Once
	x is at the second-to-last node, the last node is deleted and back is set to where x is. */
void SLL::deleteLast() {
	if (front == NULL) {
		front = back = NULL;
	}
	else if (front->next == NULL) {
		delete front;	delete back;
		front = back = NULL;
	}
	else {
		Node* x = front;
		while (x->next->next != NULL) {
			x = x->next;
		}
		delete back;
		back = x;
		x->next = NULL;
	}
}

/*  2f - This function deletes the first node in a linked list. If the list is empty, then
	front and back are set to NULL, if they weren't already. If there's only one node in the
	list, front and back are deleted and then set to NULL. If there are multiple nodes in the
	list, a new node is set at the front, front is set to the node after x, and x is deleted,
	since that was the first node. */
void SLL::deleteFront() {
	if (front == NULL) {
		front = back = NULL;
	}
	else if (front->next == NULL) {
		delete front;	delete back;
		front = back = NULL;
	}
	else {
		Node* x = front;
		front = x->next;
		delete x;
	}
}


/*  2g - This function removes all target nodes in a linked list. New nodes set at the front
	are curr and prev. If the list is empty, the function ends. Otherwise a while loop will
	be used to find target nodes at the front of the list. If the front node's data matches
	what was passed to the function, curr will be set to the front (if it wasn't already),
	front would be set to the next node after curr, and curr would be deleted. Continuing on,
	curr is re-set to the front and another while loop is used to find target nodes. As long
	as prev's next node isn't NULL, curr will go to the next node and see if that is a target.
	If it is, prev will jump over curr, curr will be deleted, and curr will go back to being
	in front of prev. If curr isn't at the target, prev will just go to the next node for
	the looping to continue. This is a traversal that allows deletion at the same time. */
void SLL::removeAllTargetNodes(int target) {
	Node* curr = front;
	Node* prev = front;
	if (front == NULL) {
		return;
	}
	while (front->data == target) {
		curr = front;
		front = curr->next;
		delete curr;
	}
	curr = front;
	while (prev->next != NULL) {
		curr = prev->next;
		if (curr->data == target) {
			prev->next = curr->next;
			delete curr;
			curr = prev->next;
		}
		else {
			prev = prev->next;
		}
	}
}
/*  2h - This function removes only the first target node in a linked list. A new node,
	curr, is set to the front of the list. If the list is empty, the function will end.
	If the front of the list is the target, which was passed to this function, then
	front will be set to after curr, which is still at the first node, and curr will be
	deleted. The function will end if this happens. Otherwise, curr will traverse through
	the list using a while loop until either it gets to the end of the list of finds the
	target node. If curr reaches the end of the list, the function will just end. If the
	target node has been found, a new node x will be set to the target node (the node after
	curr). curr will jump over x and x will be deleted. */
void SLL::removeFirstTargetNode(int target) {
	Node* curr = front;
	if (front == NULL) {
		return;
	}
	if (curr->data = target) {
		front = curr->next;
		delete curr;
		return;
	}

	curr = front;
	while ((curr->next != NULL) && (curr->next->data != target)) {
		curr = curr->next;
	}

	if (curr->next == nullptr) {
		return;
	}

	Node* x = curr->next;
	curr->next = x->next;
	delete x;
	return;
}

/*  2i - This function removes all target nodes except for the first one in a linked list.
	curr and prev are declared and set to the front of the list. If the list is empty, the
	function will end. The function will also end if there's only one node and it's the
	target node. If the first node is the target and there's more than one node in the list,
	curr will be set to the next node and prev will follow. Basically, the first node is being
	skipped, as it needs to be kept while the others are deleted in a while loop similar to the
	previous function's. If the front isn't actually the target, curr will be used to traverse
	the list until it reaches the end or finds the target node. If the end is reached, the
	function will end. Otherwise the target node has been found and will keep deleting target
	nodes until the end of the list is reached. */
void SLL::removeAllBUTFirstTargetNodes(int target) {
	Node* curr = front;
	Node* prev = front;
	if (front == NULL) {
		return;
	}
	if (front->data == target && front->next == NULL) {
		return;
	}
	if (front->data == target && front->next != NULL) {
		curr = front->next;
		prev = curr;
		while (prev->next != NULL) {
			curr = prev->next;
			if (curr->data == target) {
				prev->next = curr->next;
				delete curr;
				curr = prev->next;
			}
			else {
				prev = prev->next;
			}
		}
		return;
	}
	while (curr->data != target && curr->next != NULL) {
		curr = curr->next;
		prev = curr;
	}

	if (curr->next == nullptr) {
		return;
	}

	while (prev->next != NULL) {
		curr = prev->next;
		if (curr->data == target) {
			prev->next = curr->next;
			delete curr;
			curr = prev->next;
		}
		else {
			prev = prev->next;
		}
	}
}

/*  2j - This function returns true or false depending on whether the function is unique
	or not. A unique function won't have duplicate nodes values. 1 2 3 4 5 is unique while
	1 2 1 2 1 is not unique. If the list is empty, false is returned since a list doesn't
	exist for it to be unique. If there's one node in the list, then true is returned since
	there's technically no duplicates. Otherwise, new nodes curr and x (which were set to
	the front) are used to traverse the list multiple times, comparing node values. This
	is done with a nested while loop, where the first node will be compared to all of the
	others, then loop back so that the next node could be compared with all the others as
	well. As soon as a duplicate is found, false is returned. If no duplicates are found,
	true is returned. */
bool SLL::unique() {
	Node* curr = front;
	Node* x = front;
	if (front == NULL) {
		return false;
	}
	if (front->next == NULL) {
		return true;
	}
	while (curr->next != NULL) {
		while (x->next != NULL) {
			if (curr->data == x->data) {
				return false;
			}
			x = x->next;
		}
		curr = curr->next;
		x = curr->next;
	}
	return true;
}

int main() {
	return 0;
}