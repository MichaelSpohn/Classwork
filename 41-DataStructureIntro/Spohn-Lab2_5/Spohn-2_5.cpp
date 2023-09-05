#include <iostream>
#include <vector>
#include "Spohn-Lab2_5.h"
using namespace std;

/*  This function will take three double variables and return the product of them multiplied together. */
double boxVolume(double length, double width, double height) {
	return length * width * height;
}

/*  This function will take an integer variable and modify it by multiplying it with itself. The original
	variable will be changed in main() since this is a call-by-reference function. */
void intSqr(int& val) {
	val *= val;
}

/*  This function will take a pointer integer and multiply it with itself, just like the previous function.
	The different here is that it's call-by-pointer, so the original value in main() will be updated. */
void intSqrPntr(int* val) {
	*val *= *val;
}

/*  This function is directly from the prompt, and comments are made on each line stating what occurs. I used
	a breakpoint and went step-by-step to see what is occurring. Some of the comments may seem confusing since
	there's a lot of direct values, pointers, double pointers, and references being used. */
void function4() {
	int a = 5;						// declares a type int and sets value = 5
	int &b = a;						// declares b type int and references a, which is 5
	a = 10;							// the value of a is changed to 10, which means b is also 10 because it's a reference of a
	cout << &b << endl;				// outputs the reference variable b, which is a's memory address
	int *c = &b;					// declares c type int and points to the reference variable b
	cout << c << endl;				// outputs c, which is the memory address of b, and since b is a reference to a, it's the same address as a
	int** cPtrPtr = &c;				// declares cPtrPtr type int and double points to the memory address of c
	cout << *c << endl;				// outputs the pointer variable c, which is the value of a since c is pointing towards the value b is referencing
	cout << cPtrPtr << endl;		// outputs cPtrPtr, which is the memory address of c, which points to the memory address of a
	int d = 20;						// declares d type int and sets value = 5
	int* dPtr = &d;					// declares dPtr type int and points to the memory address of d
	int** dPtrPtr = &dPtr;			// declares dPtrPtr type int and double points to the memory address of dPtr
	dPtr = *cPtrPtr;				// the value of dPtr is set to the pointer of cPtrPtr
	cout << *dPtr << endl;			// outputs the pointer of dPtr, which is 10 because dPtr itself is equal to the pointer of cPtrPtr and that will double point to the value of a
	cout << dPtrPtr << endl;		// outputs the value of dPtrPtr, which is the memory address of dPtr
	cout << d << endl;				// outputs the value of d, which is 20
}

/*  This function will take a vector of integers using call-by-reference and reverse the function. A for loop
	is used and it's going in reverse on the vector. A new vector is used to insert the values from right-to-left
	off of the original one, and the original is overwritten with the reversed result, so main() has the reversed
	function. */
void revVect(vector<int>& vect) {
	vector<int> vectNew;

	for (int i = vect.size() - 1; i >= 0; i--) {
		vectNew.push_back(vect.at(i));
	}

	vect = vectNew;
}

/*  This is a function used to show the user what their vector looks like after inputs (and after modifications).
	A for loop is used with the vector in order to output each integer individually. */
void printVect(vector<int> vect) {
	for (int i = 0; i < vect.size(); i++) {
		cout << vect[i] << " ";
	}
	cout << endl;
}

/*  This function will take a vector of integers and reverse it in order to see if its palindrome. A vector that's
	palindrome will be the same as its original after being reversed (1 2 3 2 1). By reversing the vector and comparing
	it to its original, an if-else statement can be used to see if its palindrome. True or false will be returned. */
bool isPalindrome(vector<int> vect) {
	vector<int> vectNew = vect;
	revVect(vectNew);

	if (vect == vectNew) {
		return true;
	}
	else {
		return false;
	}
}

/*  This function will take a 2D vector of integers and reverse each row, or mirror the original vector. This is a
	call-by-reference function, so the vector will be modified and changed in main(). A for loop is used in order
	to reverse each row with the revVect() function one at a time. */
void mirrorVect(vector<vector<int>>& vect) {
	for (int i = 0; i < vect.size(); i++) {
		revVect(vect[i]);
	}
}

/*  A menu is presented to the user. The menu is in a while loop, and unless the user presses 8, the menu will reappear
	after every branch. */
int main() {
	int userInput = 0;

	while (userInput != 8) {
		cout << "Enter a value to run the corresponding function: " << endl;
		cout << "1 - Volume of a box (call-by-value)" << endl;
		cout << "2 - Square of an integer (call-by-reference)" << endl;
		cout << "3 - Square of an integer (call-by-pointer)" << endl;
		cout << "4 - Function from Lab 2.5 prompt" << endl;
		cout << "5 - Reverse a vector" << endl;
		cout << "6 - Palindrome vector test" << endl;
		cout << "7 - Flip a 2D vector from right to left (or left to right)" << endl;
		cout << "8 - End program" << endl;
		cin >> userInput;

		/*  If the user enters 1, the user will enter three values for a length, width, and height. Those values
			will be sent to boxVolume() and a result will be returned as a double. */
		if (userInput == 1) {
			double length, width, height;
			cout << "Length: ";
			cin >> length;
			cout << "Width: ";
			cin >> width;
			cout << "Height: ";
			cin >> height;

			cout << "The volume of this box is " << boxVolume(length, width, height) << endl << endl;
			system("pause");
			cout << endl;
		}

		/*  If the user enters 2, the user will enter a value to be squared in the intSqr() function. That's a
			call-by-reference function, so val will be updated here. */
		else if (userInput == 2) {
			int val;
			cout << "Enter an integer: ";
			cin >> val;

			intSqr(val);
			cout << "The square of this integer is " << val << endl << endl;
			system("pause");
			cout << endl;
		}

		/*  If the user enters 3, the user will enter a value to be squared in the intSqrPntr() function. That's
			a call-by-pointer function, so the reference of val is sent to the function so that val is updated here. */
		else if (userInput == 3) {
			int val;
			cout << "Enter an integer: ";
			cin >> val;

			intSqrPntr(&val);
			cout << "The square of this integer is " << val << endl << endl;
			system("pause");
			cout << endl;
		}

		/*  If the user enters 4, the function with the code from the prompt is called here. Nothing else occurs. */
		else if (userInput == 4) {
			function4();
			system("pause");
			cout << endl;
		}

		/*  If the user enters 5, the user will be prompted to enter integers into a vector until they enter 999. A while loop
			is used to allow the user to keep entering integers until that specific number is entered. 999 won't
			be apart of the vector since the while loop will stop accepting values before it could be apart of it.
			printVect() is then used to print the vector as is, then it's reversed with the call-by-reference function
			revVect(), then printed again with printVect(). The vector will output as reversed. */
		else if (userInput == 5) {
			vector<int> vect;
			int val = 0;
			cout << "Enter values for the vector. Enter 999 when you're finished: " << endl;
			cin >> val;

			while (val != 999) {
				vect.push_back(val);
				cin >> val;
			}

			cout << "Your vector is: ";
			printVect(vect);
			cout << "Your vector reversed is: ";
			revVect(vect);
			printVect(vect);
			cout << endl << endl;
			system("pause");
			cout << endl;
		}

		/*  If the user enters 6, the user will enter integers into a vector just like the previous branch. The vector
			will be printed with printVect() and then passed to the isPalindrome() function to see if it's a palindrome
			vector. If true is returned, then the if branch will be followed, stating that the vector is indeed palindrome. */
		else if (userInput == 6) {
			vector<int> vect;
			int val = 0;
			cout << "Enter values for the vector. Enter 999 when you're finished: " << endl;
			cin >> val;

			while (val != 999) {
				vect.push_back(val);
				cin >> val;
			}

			cout << "Your vector is: ";
			printVect(vect);
			if (isPalindrome(vect) == true) {
				cout << "Your vector is palindrome";
			}
			else {
				cout << "Your vector is not palindrome";
			}

			cout << endl << endl;
			system("pause");
			cout << endl;
		}

		/*  If the user enters 7, the user will enter values for the amount of rows and columns the 2D vector will have. Then, the
			user will enter values until the vector is full of values. A for loop is used to do this since it's a 2D vector. Another
			2D vector is used to output the rows and columns of the 2D vector. Then, the vector is sent to the mirrorVect() function,
			which is pass-by-reference, in order to reverse each row of the vector (or mirror it). Then, the same for loop is used
			to output the rows and columns of the new 2D vector. A function should've been used for printing a 2D vector since the
			same for loop is used twice. */
		else if (userInput == 7) {
			vector<vector<int>> vect;
			int rows, columns, val;
			
			cout << "Rows: ";
			cin >> rows;
			cout << "Columns: ";
			cin >> columns;
			cout << "Enter values for the 2D vector. The vector and its mirror will output once enough values have been entered: " << endl;

			for (int i = 0; i < rows; i++) {
				vector<int> temp;
				for (int j = 0; j < columns; j++) {
					cin >> val;
					temp.push_back(val);
				}
				vect.push_back(temp);
			}

			cout << "Your 2D vector: " << endl;
			for (int i = 0; i < vect.size(); i++) {
				for (int j = 0; j < vect[i].size(); j++) {
					cout << vect[i][j] << " ";
				}
				cout << endl;
			}

			mirrorVect(vect);
			cout << "Your mirrored 2D vector: " << endl;
			for (int i = 0; i < vect.size(); i++) {
				for (int j = 0; j < vect[i].size(); j++) {
					cout << vect[i][j] << " ";
				}
				cout << endl;
			}

			cout << endl;
			system("pause");
			cout << endl;
		}

		/*  If the user enters 8, the program will end by returning 0. */
		else if (userInput == 8) {
			cout << "Ending program";
			return 0;
		}

		/*  If the user enters anything else, a prompt will appear, and will return the user back to the menu. */
		else {
			cout << "Incorrect input. Please input a number 1 - 8." << endl << endl;
		}
	}
}