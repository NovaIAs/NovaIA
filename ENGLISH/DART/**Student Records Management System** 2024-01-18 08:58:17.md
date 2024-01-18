```dart
// Create a class to represent a student
class Student {
  String name;
  int age;
  double gpa;

  // Constructor
  Student(this.name, this.age, this.gpa);

  // Method to print student information
  void printInfo() {
    print('Name: $name, Age: $age, GPA: $gpa');
  }
}


main() async {
  // Create a list of students
  List<Student> students = [
    Student('John Doe', 20, 3.5),
    Student('Jane Smith', 21, 3.8),
    Student('Michael Jones', 22, 3.2)
  ];

  // Print the information of each student
  for (Student student in students) {
    student.printInfo();
  }

  // Sort the students by their GPA
  students.sort((a, b) => b.gpa.compareTo(a.gpa));

  // Print the information of the top 3 students with the highest GPA
  print('\nTop 3 students with the highest GPA:');
  for (Student student in students.take(3)) {
    student.printInfo();
  }

  // Create a method to calculate the average GPA of a list of students
  double calculateAverageGpa(List<Student> students) {
    double totalGpa = 0;
    for (Student student in students) {
      totalGpa += student.gpa;
    }
    return totalGpa / students.length;
  }

  // Calculate and print the average GPA of all students
  double averageGpa = calculateAverageGpa(students);
  print('\nAverage GPA of all students: $averageGpa');
}
```

**Explanation:**

1. We define a `Student` class to represent each student with attributes like name, age, and GPA.

2. We create a main function.

3. We create a list of Student objects representing different students.

4. We use a `for` loop to print the information of each student using the `printInfo()` method defined in the `Student` class.

5. We sort the list of students by their GPA using the `sort` method and a lambda expression.

6. We use another `for` loop to print the information of the top three students with the highest GPA.

7. We define a method called `calculateAverageGpa` to calculate the average GPA of a list of students.

8. We call the `calculateAverageGpa` method with the list of all students and store the result in the `averageGpa` variable.

9. We print the average GPA of all students.

This code demonstrates a more complex and comprehensive set of operations, including class definition, list manipulation, sorting, method definition, and function calls.