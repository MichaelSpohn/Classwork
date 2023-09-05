#include <iostream>
#include <queue>
using namespace std;

class tnode {
	public:
		tnode() { left = nullptr; right = nullptr; data = ' '; }		// Similar to DLL nodes, there's a left (prev), right (next), and a value for the node

		tnode(tnode* l, tnode* r, char d) {								// The first constructor sets the left and right of a new node to null, and data to nothing
			left = l;													// This constructor takes a character for value and tnodes for left and right
			right = r;													// So, left and right will either continue to point to null or point to another tnode
			data = d;
		}

		tnode* left;													// left, right, and data are public data members so that they can be accessed outside of the tnode class
		tnode* right;
		char data;
};

class Node {															// This node class is used for BST class
	public:
		Node(char key, int val) {										// Each node will be constructed with a key and value
			this->key = key;
			this->val = val;
			N = 1;
			left = nullptr;
			right = nullptr;
		}

		Node* left;
		Node* right;
		char key;
		int val;
		int N;
};

class BST {
	private:
		Node* root;														// root will hold the top of a BST

		Node* insert(Node* x, char key, int val) {						// Private insert that takes a node (starts with the root node), key, and value
			if (x == nullptr) return new Node(key, val);				// If the node is null, a new node with the key value is returned
			if (key < x->key) { x->left = insert(x->left, key, val); }	// If the key is smaller than the node's key, a recursive call will be done on the left child
			else if (key > x->key) { x->right = insert(x->right, key, val); }	// If the key is larger than the node's key, a recursive call will be done on the right child
			else { x->val = val; }										// If the key is the same as the node's key, then the value will be updated with the current sent value
			return x;													// The node is returned with a potentially new value (inserted into the tree)
		}

	public:
		BST() { root = nullptr; }										// This constructor sets root to null
		BST(Node* x) { root = x; }										// This constructor takes a node and has root set to it

		int search(Node* a) {											// This search function is similar to the lookup function for binary trees
			Node* x = root;												// A new node is made and set to root
			while (x != nullptr) {										// Instead of recursion, a while loop is used during the search
				if (a->key < x->key) { x = x->left; }					// If what we're looking for is smaller than the current node, then we go to the left child
				else if (a->key > x->key) { x = x->right; }				// If what we're looking for is larger than the current node, then we go to the right child
				else return x->val;										// If we've found the correct node, the value of the node is returned
			}
		}

		void insert(char key, int val) { root = insert(root, key, val); }	// Insert is done in two parts, where this public function sets the root to a call to the private function with the root node, the sent node key and value
};

int countLeaf(tnode* t) {												// A tnode is sent to countLeaf to return the amount of leaves in the tree (no children)
	int result = 0;														// result starts at 0 and could increment through recursive calls
	if (t->left == NULL && t->right == NULL) { result++; }				// A leaf is a tnode without a left and right, so if this is true, result will increase by 1
	if (t->left != NULL) { result += countLeaf(t->left); }				// The left side is looked at first if it isn't null; result will increment through recursive calls on the left tnode
	if (t->right != NULL) { result += countLeaf(t->right); }			// The right side is looked at next if it isn't null; result will increment through recursive calls on the right tnode
	return result;														// After all if statements have been looked at, the result will return to the previous self-function call or back to main
}																		// Recursion is used to traverse the left and right branches of a tnode and look for children

int depth(tnode* t) {													// Returns the depth of a tree with a root node sent to the function (the root itself doesn't count as a level)
	int left, right, val;												// These are used for adding how far deep the tree goes on the left and right sides
	if (t == NULL) { val = -1; }										// -1 is returned if there is no tree, and similarly 0 is returned if the node is all there is to the tree
	else {
		left = depth(t->left);											// left will increment on the recursive call of depth on the left side, meaning left will increase each time the left child of a node is traversed to
		right = depth(t->right);										// Similarly to left, right will increment on the recursive call of depth on the right side
		if (left > right) { val = 1 + left; }							// Whichever side went the furthest down will be used for the final value, so if left went further, left will be used
		else { val = 1 + right; }
	}
	return val;															// The depth of the tree is returned based on which side went the furthest down
}

void levelOrderScan(tnode* t) {											// This type of scan takes the root of a tree and outputs the values in order by level
	if (t == nullptr) { return; }										// The function won't continue if there is no tree
	queue<tnode*> q;													// Queue is used to store nodes in the correct order (left to right on each level)
	q.push(t);															// The root node is sent to the queue before traversal
	while (q.empty() == false) {										// Loop through the tree and add nodes to the queue until there's no more to add, output, and remove
		tnode* nn = q.front();											// A new node is used for traversal and is temporary. It will be set to the node at the front of the queue
		cout << nn->data << " ";										// The node at the front of the queue will be outputted
		q.pop();														// The node is now removed from the front of the queue, making the queue either empty or having a new front

		if (nn->left != nullptr) { q.push(nn->left); }					// If the current node has a left child, it will be pushed to the queue in order to be outputted (left before right)
		if (nn->right != nullptr) { q.push(nn->right); }				// If the current node has a right child, it will be pushed to the queue in order to be outputted (behind the left child if there was one)
	}																	// Now each node can be put into the queue to be outputted in the correct order
}																		// Once there are no more nodes to look at, the while loop will continue to output and pop until the queue is empty

bool lookup(tnode* t, char a) {											// Searches the tree for the specified character starting with the sent parent node
	if (t != nullptr) {
		if (t->data == a) { return true; }
		else { return lookup(t->left, a) || lookup(t->right, a); }
	}
	return false;
}

int size(tnode* t) {													// The size of the tree is determined by how many nodes are in it
	if (t == nullptr) { return 0; }										// If the tree is empty, the size is 0, so that will be returned and the function will end
	int result = 1;														// If the function reaches this point, there's at least 1 node in the tree, so result is declared with 1
	if (t->left != NULL) { result += size(t->left); }					// Recursion is used on the children of each node as long as there is a child, so result will increment on each node that's visited
	if (t->right != NULL) { result += size(t->right); }					// If the left child isn't available, the right will be looked at next, then return the result back to the first function call
	return result;														// After all the recursive calls, the result will be returned
}

/*  This function returns the smallest value in a tree, which if sorted, should be the first node if sorted correctly.
	The first if statement makes sure that the node has children. If it doesn't, it will return the value of the node.
	The second if statement looks at the left child, makes sure it isn't null, checks if the left node is smaller than
	the current one, and that the left child is smaller than the right. If it is, a recursive call will be done on the
	left child to check if more comparisons can be done for a smaller value. The third if statement is similar, but focues
	on the right child and checks if it's smaller than the left. In the end, a minimum value should be returned after
	any recursive calls. */
char minNode(tnode* t) {
	if (t->left == nullptr && t->right == nullptr) return t->data;
	if (t->left != nullptr && t->left->data < t->data && t->left->data < t->right->data) { minNode(t->left); }
	if (t->right != nullptr && t->right->data < t->data && t->right->data < t->left->data) { minNode(t->right); }
	return t->data;
}

int main() {
	tnode* e = new tnode(nullptr, nullptr, 'E');
	tnode* d = new tnode(nullptr, nullptr, 'D');
	tnode* c = new tnode(e, nullptr, 'C');
	tnode* b = new tnode(nullptr, d, 'B');
	tnode* a = new tnode(b, c, 'A');

	cout << "Leaves in tree: " << countLeaf(a) << endl;
	cout << "Depth of tree: " << depth(a) << endl;
	levelOrderScan(a);
	cout << endl;
	if (lookup(a, 'B') == true) { cout << "B is in the tree" << endl; }
	else { cout << "B is not in the tree" << endl; }
	if (lookup(a, 'F') == true) { cout << "F is in the tree" << endl; }
	else { cout << "F is not in the tree" << endl; }
	cout << "Size of tree: " << size(a) << endl;
	cout << "Minimum value of tree: " << minNode(a) << endl;
}