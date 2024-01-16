```groovy
// This Groovy script demonstrates a complex and differentiated code structure with a variety of features.

// Define a custom exception class for handling errors
class CustomException extends Exception {
    // Constructor
    CustomException(String message) {
        super(message)
    }
}

// Define a generic method to find the maximum element in a list
def findMax[T extends Comparable](List<T> list) {
    // Initialize the maximum value
    def max = list[0]
    // Iterate over the list and update the maximum value if a greater element is found
    for (item in list) {
        if (item > max) {
            max = item
        }
    }
    // Return the maximum value
    return max
}

// Define a method to calculate the factorial of a non-negative integer using recursion
def factorial(int n) {
    // Base case: return 1 for n = 0
    if (n == 0) {
        return 1
    }
    // Recursive case: return n * factorial(n-1)
    else {
        return n * factorial(n-1)
    }
}

// Define a method to generate a random string of a specified length
def generateRandomString(int length) {
    // Create a StringBuilder to store the random string
    def sb = new StringBuilder()
    // Iterate over the desired length
    for (i in 0..<length) {
        // Generate a random character from A to Z or a to z
        def char = (int) ('a' + Math.random() * ('z' - 'a' + 1))
        // Append the character to the StringBuilder
        sb.append((char as char))
    }
    // Return the generated random string
    return sb.toString()
}

// Define a method to convert a string to uppercase using regular expressions
def toUpperCase(String str) {
    // Use the String.replaceAll() method with a regular expression to convert lowercase letters to uppercase
    return str.replaceAll(/[a-z]/, str -> str.toUpperCase())
}

// Define a method to calculate the area of a triangle given its base and height
def triangleArea(double base, double height) {
    // Calculate the area using the formula: (base * height) / 2
    return (base * height) / 2
}

// Define a method to find the roots of a quadratic equation (ax^2 + bx + c = 0)
def quadraticRoots(double a, double b, double c) {
    // Calculate the discriminant (b^2 - 4ac)
    def discriminant = b * b - 4 * a * c
    // Check the discriminant to determine the nature of the roots
    if (discriminant < 0) {
        throw new CustomException("The equation has no real roots.")
    }
    else if (discriminant == 0) {
        // If the discriminant is zero, there is one real root (a.k.a. double root)
        def root = (-b) / (2 * a)
        return [root]
    }
    else {
        // If the discriminant is positive, there are two real roots
        def root1 = (-b + Math.sqrt(discriminant)) / (2 * a)
        def root2 = (-b - Math.sqrt(discriminant)) / (2 * a)
        return [root1, root2]
    }
}

// Define a method to create a Person object with name and age
def createPerson(String name, int age) {
    // Create a new Person object
    def person = [name: name, age: age]
    // Return the Person object
    return person
}

// Define a method to sort a list of Person objects by age
def sortByAge(List<Person> people) {
    // Sort the list using the Person.age property as the sorting criterion
    return people.sort { person1, person2 -> person1.age <=> person2.age }
}

// Define a method to filter a list of Person objects based on a given age
def filterByAge(List<Person> people, int age) {
    // Filter the list using the Person.age property as the filtering criterion
    return people.findAll { person -> person.age == age }
}

// Define a method to print the elements of a list
def printList(List list) {
    // Iterate over the list and print each element
    for (item in list) {
        println item
    }
}

// Define a class representing a Student with name, age, and GPA
class Student {
    String name
    int age
    double gpa
}

// Define a method to create a Student object with name, age, and GPA
def createStudent(String name, int age, double gpa) {
    // Create a new Student object
    def student = new Student(name: name, age: age, gpa: gpa)
    // Return the Student object
    return student
}

// Define a method to sort a list of Student objects by GPA
def sortByGPA(List<Student> students) {
    // Sort the list using the Student.gpa property as the sorting criterion
    return students.sort { student1, student2 -> student1.gpa <=> student2.gpa }
}

// Define a method to filter a list of Student objects based on a given GPA
def filterByGPA(List<Student> students, double gpa) {
    // Filter the list using the Student.gpa property as the filtering criterion
    return students.findAll { student -> student.gpa >= gpa }
}

// Define a method to print the elements of a list of Student objects
def printStudentList(List<Student> students) {
    // Iterate over the list and print each student's information
    for (student in students) {
        println "Name: ${student.name}, Age: ${student.age}, GPA: ${student.gpa}"
    }
}

// Define a method to calculate the average GPA of a list of Student objects
def calculateAverageGPA(List<Student> students) {
    // Calculate the total GPA by summing the GPAs of all students
    def totalGPA = students.sum { student -> student.gpa }
    // Calculate the average GPA by dividing the total GPA by the number of students
    def averageGPA = totalGPA / students.size()
    // Return the average GPA
    return averageGPA
}

// Define a method to find the Student object with the highest GPA in a list
def findStudentWithHighestGPA(List<Student> students) {
    // Find the maximum GPA using the findMax method
    def maxGPA = findMax(students.collect { student -> student.gpa })
    // Filter the list to find students with the maximum GPA
    def studentsWithMaxGPA = students.findAll { student -> student.gpa == maxGPA }
    // Return the first student with the maximum GPA (assuming there is only one)
    return studentsWithMaxGPA[0]
}

// Define a method to create a Map representing a phone book with names and phone numbers
def createPhoneBook(List<String> names, List<String> phoneNumbers) {
    // Check if the number of names and phone numbers match
    if (names.size() != phoneNumbers.size()) {
        throw new CustomException("The number of names and phone numbers must be the same.")
    }
    // Create a Map to store the phone book entries
    def phoneBook = [:]
    // Iterate over the names and phone numbers and add them to the phone book
    for (i in 0..<names.size()) {
        phoneBook[names[i]] = phoneNumbers[i]
    }
    // Return the phone book Map
    return phoneBook
}

// Define a method to search the phone book for a given name and return the corresponding phone number
def findPhoneNumber(Map phoneBook, String name) {
    // Check if the name exists in the phone book
    if (!phoneBook.containsKey(name)) {
        throw new CustomException("The name ${name} is not found in the phone book.")
    }
    // Return the phone number associated with the name
    return phoneBook[name]
}

// Define a class representing a Book with title, author, and price
class Book {
    String title
    String author
    double price
}

// Define a method to create a Book object with title, author, and price
def createBook(String title, String author, double price) {
    // Create a new Book object
    def book = new Book(title: title, author: author, price: price)
    // Return the Book object