```scala
// Define a custom class to represent a student
class Student(val name: String, val age: Int, val gpa: Double) {

  // Define a method to calculate the student's letter grade
  def calculateLetterGrade(): String = {
    if (gpa >= 3.8) "A"
    else if (gpa >= 3.5) "B+"
    else if (gpa >= 3.2) "B"
    else if (gpa >= 2.8) "C+"
    else if (gpa >= 2.5) "C"
    else if (gpa >= 2.2) "D+"
    else if (gpa >= 2.0) "D"
    else "F"
  }

  // Define a method to print the student's information
  def printInfo(): Unit = {
    println(s"Name: $name")
    println(s"Age: $age")
    println(s"GPA: $gpa")
    println(s"Letter Grade: ${calculateLetterGrade()}")
  }
}

// Define a companion object for the Student class
object Student {

  // Define a method to create a list of students from a CSV file
  def createStudentsFromCSV(fileName: String): List[Student] = {
    val lines = scala.io.Source.fromFile(fileName).getLines().toList
    lines.map(line => {
      val parts = line.split(",")
      new Student(parts(0), parts(1).toInt, parts(2).toDouble)
    })
  }

  // Define a method to print the information for a list of students
  def printStudents(students: List[Student]): Unit = {
    students.foreach(_.printInfo())
  }
}

// Create a main object to test the Student class
object Main {

  def main(args: Array[String]): Unit = {
    // Create a list of students from a CSV file
    val students = Student.createStudentsFromCSV("students.csv")

    // Print the information for each student
    Student.printStudents(students)
  }
}
```

Explanation:

1. We define a custom class called `Student` to represent a student. It has three properties: `name`, `age`, and `gpa`.

2. Inside the `Student` class, we define a method called `calculateLetterGrade()` that calculates the student's letter grade based on their GPA.

3. We also define a method called `printInfo()` that prints the student's information.

4. We define a companion object for the `Student` class, which contains two methods:

   - `createStudentsFromCSV()`: This method reads a CSV file containing student information and creates a list of `Student` objects from it.
   - `printStudents()`: This method prints the information for a list of students.

5. In the `main()` method of the `Main` object, we create a list of students by calling the `createStudentsFromCSV()` method.

6. We then print the information for each student by calling the `printStudents()` method.

This code demonstrates the use of classes, objects, methods, and file I/O in Scala to create a program that reads student information from a CSV file and prints it out.