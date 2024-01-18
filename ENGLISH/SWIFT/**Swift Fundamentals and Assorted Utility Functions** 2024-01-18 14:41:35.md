```swift
// Complex Swift Code

// Import Foundation library
import Foundation

// Function to print a message
func printMessage(_ message: String) {
    // Print a message to the console
    print(message)
}

// Function to calculate the factorial of a number
func factorial(_ number: Int) -> Int {
    // Base case: if the number is 0 or 1, return 1
    if number == 0 || number == 1 {
        return 1
    }
    // Recursive case: calculate the factorial by multiplying the number with the factorial of the previous number
    else {
        return number * factorial(number - 1)
    }
}

// Function to check if a number is prime
func isPrime(_ number: Int) -> Bool {
    // If the number is 1, it is not prime
    if number == 1 {
        return false
    }
    // Iterate from 2 to the square root of the number
    for i in 2...Int(sqrt(Double(number))) {
        // If the number is divisible by any number from 2 to its square root, it is not prime
        if number % i == 0 {
            return false
        }
    }
    // If the number is divisible by no number from 2 to its square root, it is prime
    return true
}

// Function to generate a random number
func random() -> Int {
    // Create an instance of the Mersenne Twister random number generator
    let generator = SystemRandomNumberGenerator()
    // Generate a random number between 0 and 1
    return generator.nextInt(in: 0...1000)
}

// Class to represent a student
class Student {
    // Properties
    var name: String
    var grade: Int
    
    // Constructor
    init(name: String, grade: Int) {
        self.name = name
        self.grade = grade
    }
    
    // Function to print the student's information
    func printInfo() {
        print("Name: \(name), Grade: \(grade)")
    }
}

// Create an array of students
var students: [Student] = []

// Add some students to the array
students.append(Student(name: "John", grade: 90))
students.append(Student(name: "Mary", grade: 85))
students.append(Student(name: "Bob", grade: 75))

// Print the information of each student
for student in students {
    student.printInfo()
}

// Function to sort the students by their grade
func sortStudentsByGrade(students: [Student]) -> [Student] {
    // Sort the students using the sorted() function with a custom closure
    return students.sorted { $0.grade > $1.grade }
}

// Sort the students by their grade
let sortedStudents = sortStudentsByGrade(students: students)

// Print the information of each sorted student
for student in sortedStudents {
    student.printInfo()
}

// Call the printMessage function
printMessage("Hello, world!")

// Call the factorial function
let result = factorial(5)
print("Factorial of 5: \(result)")

// Call the isPrime function
let isPrimeResult = isPrime(17)
print("Is 17 prime? \(isPrimeResult)")

// Call the random function
let randomNumber = random()
print("Random number: \(randomNumber)")
```

Explanation:
This Swift code is a complex and differentiated code that includes various functions and classes to perform different tasks. Let's break down the code:

1. Function `printMessage`: This function takes a message as an argument and prints it to the console.

2. Function `factorial`: This function calculates the factorial of a given number recursively. The factorial of a number is the product of all positive integers less than or equal to that number.

3. Function `isPrime`: This function checks if a given number is prime or not. A prime number is a natural number greater than 1 that is not a product of two smaller natural numbers.

4. Function `random`: This function generates a random integer between 0 and 1000 using the Mersenne Twister random number generator.

5. Class `Student`: This class represents a student with properties `name` and `grade`. It also has a method `printInfo` to print the student's information.

6. Array of Students: An array of `Student` objects is created to store student information.

7. Function `sortStudentsByGrade`: This function sorts the array of students based on their grade in descending order using the `sorted()` function and a custom closure.

8. Sorting Students: The `sortStudentsByGrade` function is called to sort the students by their grade, and the sorted array is then printed.

9. Calling Functions: The `printMessage`, `factorial`, `isPrime`, and `random` functions are called to demonstrate their usage.