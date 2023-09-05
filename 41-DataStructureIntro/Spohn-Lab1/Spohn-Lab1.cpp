#include <iostream>
#include <cmath>
#include <vector>
#include <string>
#include "Spohn-Lab1.h"
using namespace std;

bool isLeapYear(int num) {
	/*  This function will return either true or false depening on whether of not num is a leap year,
		following the mathematical conditions in the if-else branches below. Leap years occur every
		4 years with some exceptions, which is why there is more than one condition. If num is divisible
		by 4, then it will continue to the next condition. If it's not divisible by 100 but divisible by 400,
		then it can return true. The algorithm was used directly from the Wikipedia page about leap years:
		https://en.wikipedia.org/wiki/Leap_year */

	if (num % 4 != 0) {
		return false;
	}
	else if (num % 100 != 0) {
		return true;
	}
	else if (num % 400 != 0) {
		return false;
	}
	else {
		return true;
	}
}

void ChineseZodiac(int num) {
	/*  This function will cout the Chinese Zodiac depending on which case is used. The switch formula uses
		num to obtain a remainder. The remainder is important for getting the correct result, as each zodiac
		cycles around through all 12. This isn't the most accurate as the Chinese Zodiac Wikipedia page, but
		it's accurate to the year. This algorithm was put together after researching different ways of actually
		calculating the Chinese Zodiac with a given year. */

	switch ((num - 4) % 12) {
		case 0:
			cout << "Rat";
			break;
		case 1:
			cout << "Ox";
			break;
		case 2:
			cout << "Tiger";
			break;
		case 3:
			cout << "Rabbit";
			break;
		case 4:
			cout << "Dragon";
			break;
		case 5:
			cout << "Snake";
			break;
		case 6:
			cout << "Horse";
			break;
		case 7:
			cout << "Goat";
			break;
		case 8:
			cout << "Monkey";
			break;
		case 9:
			cout << "Rooster";
			break;
		case 10:
			cout << "Dog";
			break;
		case 11:
			cout << "Pig";
			break;
	}
}

int findMin(vector<int> vect) {
	/*  This function takes a vector with integers and finds the minimum value with the use of a temporary integer
		holder. The temporary holder will hold the first value of the vector, and as the for loop cycles through
		the vector, if any integer is less than what's being held, the temporary holder would change to the lesser
		value. At the end, the holder is what is returned, since it will hold the smallest value. */

	int tempVal, i;
	tempVal = vect.at(0);

	for (i = 0; i < vect.size(); i++) {
		if (vect.at(i) < tempVal) {
			tempVal = vect.at(i);
		}
	}

	return tempVal;
}

int stringCalc(string input) {
	/*  This function takes a string input and converts the numbers in the string to their own separate integer values
		so that they could be added, subtracted, multiplied, or divided. There are a few variables used to do this
		correctly:
		- location > the certain spot in the string where the operator is located
		- strLeft > the part of the original string that is of the first number
		- strRight > the part of the original string that is after the operator
		- numLeft > the number left of the operator
		- numRight > the number right of the operator
		By using the location of the operator, the numbers can be separated and converted into integers with stoi().
		Depending on which operator was found in the string (with the help of an if-else branch), the numbers will
		then be mathematically calculated and the result will be returned. There are a few drawbacks with this function...
		Only one operator can be used, only two operands can be used, and if letters or other characters are thrown in, errors
		could occur. But the basics can be done with this function.*/
	int location, numLeft, numRight;
	string strLeft, strRight;

	if (input.find('+') != string::npos) {
		location = input.find('+');
		strLeft = input.substr(0, location);
		numLeft = stoi(strLeft);
		strRight = input.erase(0, location + 1);
		numRight = stoi(strRight);
		return (numLeft + numRight);
	}
	else if (input.find('-') != string::npos) {
		location = input.find('-');
		strLeft = input.substr(0, location);
		numLeft = stoi(strLeft);
		strRight = input.erase(0, location + 1);
		numRight = stoi(strRight);
		return (numLeft - numRight);
	}
	else if (input.find('*') != string::npos) {
		location = input.find('*');
		strLeft = input.substr(0, location);
		numLeft = stoi(strLeft);
		strRight = input.erase(0, location + 1);
		numRight = stoi(strRight);
		return (numLeft * numRight);
	}
	else if (input.find('/') != string::npos) {
		location = input.find('/');
		strLeft = input.substr(0, location);
		numLeft = stoi(strLeft);
		strRight = input.erase(0, location + 1);
		numRight = stoi(strRight);
		return (numLeft / numRight);
	}
	else {
		cout << "Error: Missing operator" << endl;
		return 0;
	}
}

void vectAvg(vector<vector<int>> vect, int rows, int columns) {
	/*  This function will take a 2D vector as well as the amount of rows and columns in it to calculate and
		output the average. First, the rows and columns are used to find the size of the vector through
		multiplication. Then, it's a simple matter of adding each integer together and dividing that sum with
		the vector's size. A lot of the prep work for the 2D vector is done in main(). */

	int sum = 0;
	int size = rows * columns;
	double avg;

	for (int i = 0; i < vect.size(); i++) {
		for (int j = 0; j < vect[i].size(); j++) {
			sum += vect[i][j];
		}
	}
	
	avg = static_cast<double>(sum) / static_cast<double>(size);
	cout << "The average of the 2D vector is " << avg << endl << endl;
}

int main() {
	/*  This program provides the user with a list of functions that they may use by inputting the correct number.
		Numbers 1 - 5 will utilize the if-else branch and go towards using a function. Inputting -1 will immediately
		terminate the program. If any other numbers are entered, a warning will appear, and the menu will come back
		up for the user. There are variables created in each if-else scope if a function requires them. */

	int userInput = 0;

	while (userInput != -1) {
		cout << "To use one of the following functions, enter the corresponding number:" << endl;
		cout << "1 - Leap Year" << endl;
		cout << "2 - Chinese Zodiac" << endl;
		cout << "3 - Minimum Value of a Vector" << endl;
		cout << "4 - Math Calculation from a String Input" << endl;
		cout << "5 - Average of a 2D Vector" << endl;
		cout << "-1 - Exits the program" << endl;
		cout << "Please enter a number: ";
		cin >> userInput;
		cout << endl;

		if (userInput == 1) {
			/*  A year is entered by the user in order to use the isLeapYear() function. There are two outputs
				that could be used depending on whether or not the entered year is a leap year. */

			bool leapYearResult;
			int leapYearInput = 0;

			cout << "Enter a year: ";
			cin >> leapYearInput;
			leapYearResult = isLeapYear(leapYearInput);
			if (leapYearResult == true) {
				cout << leapYearInput << " is a leap year" << endl << endl;
			}
			else {
				cout << leapYearInput << " is NOT a leap year" << endl << endl;
			}
			system("pause");
			cout << endl;
		}
		else if (userInput == 2) {
			/*  The user enters a year to be used with the ChineseZodiac() function, which will return a string
				to add to the cout in this scope. */

			int zodiacYearInput = 0;

			cout << "Enter a year: ";
			cin >> zodiacYearInput;
			cout << zodiacYearInput << " is the year of the ";
			ChineseZodiac(zodiacYearInput);
			cout << endl << endl;
			system("pause");
			cout << endl;
		}
		else if (userInput == 3) {
			/*  A vector is declared as well as a variable so that the user can add integers to the vector.
				In order to stop accepting integers, the user can enter 999 in order to stop the while loop
				that allows push_backs with the vector. Then, the vector is sent to the findMin() function and
				will return with an integer result. */

			vector<int> vect;
			int val;
			cout << "Enter integers for the vector. Enter 999 to when you are done:" << endl;
			cin >> val;
			while (val != 999) {
				vect.push_back(val);
				cin >> val;
			}
			cout << "The minimum value of the vector is " << findMin(vect) << endl << endl;
			system("pause");
			cout << endl;
		}
		else if (userInput == 4) {
			/*  The user inputs a string with a number on the left, an operator, and a number on the right.
				It's very easy to mess this one up since it has to be THAT specific so that the stringCalc()
				function can read and convert it properly, returning an int. */
			string calcInput;
			
			cout << "Enter a string for mathematical calculation. Don't use spaces and use one operator: ";
			cin >> calcInput;
			cout << calcInput << " = " << stringCalc(calcInput) << endl << endl;
			system("pause");
			cout << endl;
		}
		else if (userInput == 5) {
			/*  A 2D vector is declared along with variables so the user can populate the vector and
				state how many rows and columns it will have. Rows != columns, so there's a while loop to
				verify that they are not equal. The user will then input values until the vector can't hold
				anymore (which is dependent on the rows and columns the user decided on). The vector is then
				outputted so the user can see how it looks as a 2D vector, then it's sent to the vectAvg()
				function with the rows and columns variables in order to compute the average. */

			vector<vector<int>> vect;
			int val, rows, columns;
			bool valid = false;

			while (valid == false) {
				cout << "How many rows would you like in the 2D vector: ";
				cin >> rows;
				cout << "How many columns would you like in the 2D vector: ";
				cin >> columns;

				if (rows == columns) {
					cout << "Error: The amount of rows CANNOT equal the amount of columns" << endl;
				}
				else {
					valid = true;
				}
			}

			cout << "Enter integers for the 2D vector. The vector and its average will output when enough integers are entered:" << endl;

			for (int i = 0; i < rows; i++) {
				vector<int> temp;
				for (int j = 0; j < columns; j++) {
					cin >> val;
					temp.push_back(val);
				}
				vect.push_back(temp);
			}

			cout << "Here is the 2D vector:" << endl;

			for (int i = 0; i < vect.size(); i++) {
				for (int j = 0; j < vect[i].size(); j++) {
					cout << vect[i][j] << " ";
				}
				cout << endl;
			}

			vectAvg(vect, rows, columns);
			system("pause");
			cout << endl;
		}
		else if (userInput == -1) {
			/*  If the user enters -1, the program ends. */

			cout << "Program terminated" << endl;
		}
		else {
		/*  If the user enters something other than 1 - 5 or -1, then this warning will popup to inform the user. */

			cout << "Warning: Incorrect input. Please enter a number 1 - 5 to use a function, -1 to exit." << endl << endl;
		}
	}
}