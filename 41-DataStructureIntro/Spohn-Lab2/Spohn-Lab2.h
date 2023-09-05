#pragma once
using namespace std;

class Date {
	public:
		Date();
		Date(int, int, int);
		void setMonth(int);
		void setDay(int);
		void setYear(int);
		int getMonth();
		int getDay();
		int getYear();
	private:
		int month;
		int day;
		int year;
};

class Time {
	public:
		Time();
		Time(int, int);
		void setHour(int);
		void setMin(int);
		int getHour();
		int getMin();
	private:
		int hour;
		int minute;
};

class Invitee {
	public:
		Invitee();
		~Invitee();
		bool isValidEmail(string);
		void setEmail(string);
		string getEmail();
	private:
		string email;
};

class Event {
	public:
		Event();
		Event(string, int, int, int, int, int, int, string);
		~Event();
		void invitePeople();
		void printEventInfo();
		int getMonth();
	private:
		string title;
		int month;
		int day;
		int year;
		int startHour;
		int startMin;
		int endHour;
		int endMin;
		vector<string>* invites;
		int duration;
		string location;
};

class Calendar {
	public:
		void addEvent(Event*);
		void deleteEvent(Event*);
		void searchByMonth(int);
	private:
		vector<Event*> events;
};