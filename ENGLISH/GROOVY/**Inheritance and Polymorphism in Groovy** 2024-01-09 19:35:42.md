```groovy
// Define a class to represent a Person
class Person {
    String name
    int age
    String occupation

    // Constructor to initialize the Person object
    Person(String name, int age, String occupation) {
        this.name = name
        this.age = age
        this.occupation = occupation
    }

    // Method to print the details of the Person
    void printDetails() {
        println "Name: $name"
        println "Age: $age"
        println "Occupation: $occupation"
    }

    // Method to check if the Person is an adult (age >= 18)
    boolean isAdult() {
        return age >= 18
    }
}

// Define a class to represent a Student
class Student extends Person {
    String schoolName
    String major

    // Constructor to initialize the Student object
    Student(String name, int age, String occupation, String schoolName, String major) {
        super(name, age, occupation)  // Call the constructor of the parent class
        this.schoolName = schoolName
        this.major = major
    }

    // Method to print the details of the Student
    @Override
    void printDetails() {
        super.printDetails()  // Call the printDetails() method of the parent class
        println "School Name: $schoolName"
        println "Major: $major"
    }

    // Method to check if the Student is a graduate (age >= 22)
    boolean isGraduate() {
        return age >= 22
    }
}

// Define a class to represent a Teacher
class Teacher extends Person {
    String schoolName
    String subject

    // Constructor to initialize the Teacher object
    Teacher(String name, int age, String occupation, String schoolName, String subject) {
        super(name, age, occupation)  // Call the constructor of the parent class
        this.schoolName = schoolName
        this.subject = subject
    }

    // Method to print the details of the Teacher
    @Override
    void printDetails() {
        super.printDetails()  // Call the printDetails() method of the parent class
        println "School Name: $schoolName"
        println "Subject: $subject"
    }

    // Method to check if the Teacher is a professor (age >= 30)
    boolean isProfessor() {
        return age >= 30
    }
}

// Create a list of Person objects
def people = [
    new Person("John Doe", 25, "Software Engineer"),
    new Student("Jane Smith", 20, "Student", "Stanford University", "Computer Science"),
    new Teacher("Michael Jones", 35, "Teacher", "Harvard University", "Mathematics")
]

// Print the details of each Person in the list
people.each {
    println "Person Details:"
    it.printDetails()
    println()  // Add a blank line between each Person's details
}

// Check if each Person in the list is an adult
people.each {
    println "$it.name is an adult: ${it.isAdult()}"
}

// Check if each Student in the list is a graduate
people.findAll { it instanceof Student }.each {
    println "$it.name is a graduate: ${it.isGraduate()}"
}

// Check if each Teacher in the list is a professor
people.findAll { it instanceof Teacher }.each {
    println "$it.name is a professor: ${it.isProfessor()}"
}
```

Explanation:

1. We define three classes: `Person`, `Student`, and `Teacher`. `Person` is the parent class, and `Student` and `Teacher` are subclasses that inherit from `Person`.

2. Each class has its own properties and methods. For example, the `Person` class has properties for `name`, `age`, and `occupation`, and a method to print the person's details.

3. The `Student` class extends the `Person` class and adds properties for `schoolName` and `major`, as well as a method to check if the student is a graduate.

4. The `Teacher` class also extends the `Person` class and adds properties for `schoolName` and `subject`, as well as a method to check if the teacher is a professor.

5. We create a list of `Person` objects, including a `Person`, a `Student`, and a `Teacher`.

6. We use the `each()` method to iterate over the list of people and print the details of each person.

7. We also use `findAll()` and `each()` to check if each person is an adult, if each student is a graduate, and if each teacher is a professor.

This code demonstrates the use of inheritance, polymorphism, and some common Groovy constructs like closures and method chaining. It also shows how to define custom classes and objects in Groovy.