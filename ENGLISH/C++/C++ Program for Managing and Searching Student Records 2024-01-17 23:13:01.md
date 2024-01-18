```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <string>
#include <sstream>
#include <fstream>

using namespace std;

// Define a custom data structure to store information about a student
struct Student {
  string name;
  int age;
  double gpa;
  vector<string> courses;
};

// Define a function to compare two students based on their names
bool compareStudentsByName(const Student& s1, const Student& s2) {
  return s1.name < s2.name;
}

// Define a function to compare two students based on their ages
bool compareStudentsByAge(const Student& s1, const Student& s2) {
  return s1.age < s2.age;
}

// Define a function to compare two students based on their GPAs
bool compareStudentsByGPA(const Student& s1, const Student& s2) {
  return s1.gpa > s2.gpa;
}

// Define a function to print information about a student
void printStudent(const Student& student) {
  cout << "Name: " << student.name << endl;
  cout << "Age: " << student.age << endl;
  cout << "GPA: " << student.gpa << endl;
  cout << "Courses: ";
  for (const string& course : student.courses) {
    cout << course << " ";
  }
  cout << endl;
}

// Define a function to read student information from a file
vector<Student> readStudentsFromFile(const string& filename) {
  vector<Student> students;
  ifstream file(filename);
  if (file.is_open()) {
    string line;
    while (getline(file, line)) {
      stringstream ss(line);
      Student student;
      ss >> student.name;
      ss >> student.age;
      ss >> student.gpa;
      string course;
      while (ss >> course) {
        student.courses.push_back(course);
      }
      students.push_back(student);
    }
    file.close();
  }
  return students;
}

// Define a function to sort students based on a given comparison function
void sortStudents(vector<Student>& students, bool (*compare)(const Student&, const Student&)) {
  sort(students.begin(), students.end(), compare);
}

// Define a function to find students with a given name using a linear search
vector<Student> findStudentsByName(const vector<Student>& students, const string& name) {
  vector<Student> results;
  for (const Student& student : students) {
    if (student.name == name) {
      results.push_back(student);
    }
  }
  return results;
}

// Define a function to find students with a given age using a binary search
vector<Student> findStudentsByAge(const vector<Student>& students, int age) {
  vector<Student> results;
  auto it = lower_bound(students.begin(), students.end(), age, compareStudentsByAge);
  while (it != students.end() && it->age == age) {
    results.push_back(*it);
    it++;
  }
  return results;
}

// Define a function to find students with a given GPA using a set
set<Student> findStudentsByGPA(const vector<Student>& students, double gpa) {
  set<Student> results;
  for (const Student& student : students) {
    if (student.gpa >= gpa) {
      results.insert(student);
    }
  }
  return results;
}

// Define a function to find students taking a given course using an unordered_set
unordered_set<Student> findStudentsTakingCourse(const vector<Student>& students, const string& course) {
  unordered_set<Student> results;
  for (const Student& student : students) {
    if (find(student.courses.begin(), student