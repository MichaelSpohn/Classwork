#include <iostream>
#include <string>
#include <vector>
#include <stdio.h>
#include "Spohn-Lab2.h"
using namespace std;

// --- Date functions ---
/*  Date constructor: month, day, year set to 0.*/
Date::Date() {
	month = 0;
	day = 0;
	year = 0;
}

/*  This constructor function takes three ints for the month, day, and year of a date and sets
	them to their respective private data members. */
Date::Date(int month, int day, int year) {
	setMonth(month);
	setDay(day);
	setYear(year);
}

/*  This function will take an integer and set it to the private month data member. */
void Date::setMonth(int month) {
	this->month = month;
}

/*  This function will take an integer and set it to the private day data member. */
void Date::setDay(int day) {
	this->day = day;
}

/*  This function will take an integer and set it to the private year data member. */
void Date::setYear(int year) {
	this->year = year;
}

/*  This function returns the value of month. */
int Date::getMonth() {
	return month;
}

/*  This function returns the value of day. */
int Date::getDay() {
	return day;
}

/*  This function returns the value of year. */
int Date::getYear() {
	return year;
}

// --- Time functions ---
/*  Constructor: sets hour and minute to nothing. */
Time::Time() {
	hour = 0;
	minute = 0;
}

/*  This constructor function takes two integers for a time's hour and minute and sets them
	to their respective private data members. */
Time::Time(int hour, int minute) {
	setHour(hour);
	setMin(minute);
}

/*  This function will take an integer and set it to the private hour data member. */
void Time::setHour(int hour) {
	this->hour = hour;
}

/*  This function will take an integer and set it to the private minute data member. */
void Time::setMin(int minute) {
	this->minute = minute;
}

/*  This function returns the value of hour. */
int Time::getHour() {
	return hour;
}

/*  This function returns the value of minute. */
int Time::getMin() {
	return minute;
}

// --- Invitee functions ---
/*  Constructor: Sets email to nothing. */
Invitee::Invitee() { email = ""; }

/*  Destructor: Deletes any information in the class. */
Invitee::~Invitee() {}

/*  This function will take a string and verify if it starts with a letter and is at least three characters
	in length. */
bool Invitee::isValidEmail(string email) {
	if ((isalpha(email.at(0) == true)) && (email.size() >= 3) && (email.find('@') == true)) {
		return true;
	}
	else {
		return false;
	}
}

/*  This function will take a string and set it to the private email data member if it's valid. */
void Invitee::setEmail(string email) {
	if (isValidEmail(email) == true) {
		this->email = email;
	}
	else {
		cout << "Invalid email" << endl;
	}
}

/*  This function returns the set string for email. */
string Invitee::getEmail() {
	return email;
}

// --- Event functions ---
/*  Constructor: Sets title, month, day, year, start hour/min, end hour/min, invites, duration, and location to nothing. */
Event::Event() {
	title = "";
	month = 0;
	day = 0;
	year = 0;
	startHour = 0;
	startMin = 0;
	endHour = 0;
	endMin = 0;
	invites = nullptr;
	duration = 0;
	location = "";
}

/*  This constructor function sets all of the private data members using inputs from the user in main. There are date and time
	private data members that are set with their respective functions. The end time formula was referenced from the instructor's version
	of this lab. The invites data member is set to be a new vector. */
Event::Event(string title, int month, int day, int year, int startHour, int startMin, int duration, string location) {
	this->title = title;
	this->month = month;
	this->day = day;
	this->year = year;
	this->startHour = startHour;
	this->startMin = startMin;
	this->duration = duration;

	int length = startMin + duration;
	endMin = length % 60;
	endHour = startHour + (length / 60);

	this->location = location;
	invites = new vector<string>();
}

/*  Destructor: Deletes invites. */
Event::~Event() {
	delete invites;
}

/*  This function will allow the user to enter email addresses until they enter 999. The while loop will check if the user
	entered 999, and if they didn't, the setEmail() function will be called and will verify the email is in the correct format.
	If the email is okay, then it's added to the vector, which was set up in the second constructor function. */
void Event::invitePeople() {
	Invitee email;
	string userInput = "";

	cout << "Enter the email addresses of those you want to invite. Enter 999 when you're finished." << endl;
	cin >> userInput;
	while (userInput != "999") {
		email.setEmail(userInput);
		invites->push_back(email.getEmail());
		cin >> userInput;
	}
}

/*  This function will print out all of the information that was gathered. Since invites is a vector, a for loop is used to
	print out every email in that vector. */
void Event::printEventInfo() {
	cout << "Title: " << title << endl;
	cout << "Date: " << month << "/" << day << "/" << year << endl;
	cout << "Time: " << startHour << ":" << startMin << " - " << endHour << ":" << endMin << endl;
	cout << "Duration: " << duration << endl;
	cout << "Location: " << location << endl;
	cout << "Invitees: " << endl;
	for (int i = 0; i < invites->size(); i++) {
		cout << invites->at(i) << endl;
	}
	cout << endl;
}

int Event::getMonth() {
	return month;
}

// --- Calendar functions ---
/*  This function takes an event variable and adds it to the Event vector. */
void Calendar::addEvent(Event* event) {
	events.push_back(event);
}

/*  This function takes an event variable and deletes it from the Event vector IF it exists.
	In order to see if the event exists, a for loop with a nested if-statement is used. The for
	loop will cycle through the vector and will break out if the event is found. Once it's found,
	it will be deleted, but if it isn't, then a warning message will output stating that it couldn't
	be found in the vector of events. */
void Calendar::deleteEvent(Event* event) {
	bool real;
	int i;

	for (i = 0; i < events.size(); i++) {
		if (event == events.at(i)) {
			real = true;
			break;
		}
		else {
			real = false;
		}
	}

	if (real == true) {
		cout << "Deleting the following event:" << endl;
		event->printEventInfo();
		events.erase(events.begin() + i);
	}
	else {
		cout << "Event not found." << endl;
	}
}

/*  This function searches for events by month. A for loop with a nested if-else statement is used.
	The variable passed to this function is the month that'll be used for searching. If there is an
	event that has the month that was passed to this function, the event information will be printed.
	If there are no events with the requested month, then nothing will be printed. */
void Calendar::searchByMonth(int month) {
	for (int i = 0; i < events.size(); i++) {
		if (events.at(i)->getMonth() == month) {
			events.at(i)->printEventInfo();
		}
		else {
			continue;
		}
	}
	cout << endl;
}