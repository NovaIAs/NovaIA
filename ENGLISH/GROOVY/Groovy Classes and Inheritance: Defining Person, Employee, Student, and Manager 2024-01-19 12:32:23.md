```groovy
// Define a Groovy class called "Person"
class Person {

    // Define properties of the "Person" class
    String name
    int age
    String occupation

    // Define a constructor for the "Person" class
    Person(String name, int age, String occupation) {
        this.name = name
        this.age = age
        this.occupation = occupation
    }

    // Define a method called "introduce" for the "Person" class
    void introduce() {
        println "Hello, my name is ${name}, I am ${age} years old, and I am a ${occupation}."
    }
}

// Define a Groovy class called "Employee" that extends the "Person" class
class Employee extends Person {

    // Define properties of the "Employee" class
    int employeeId
    String department

    // Define a constructor for the "Employee" class
    Employee(String name, int age, String occupation, int employeeId, String department) {
        super(name, age, occupation)
        this.employeeId = employeeId
        this.department = department
    }

    // Override the "introduce" method from the "Person" class
    @Override
    void introduce() {
        super.introduce()
        println "I am an employee with ID ${employeeId} and I work in the ${department} department."
    }
}

// Define a Groovy class called "Student" that extends the "Person" class
class Student extends Person {

    // Define properties of the "Student" class
    String school
    String major

    // Define a constructor for the "Student" class
    Student(String name, int age, String occupation, String school, String major) {
        super(name, age, occupation)
        this.school = school
        this.major = major
    }

    // Override the "introduce" method from the "Person" class
    @Override
    void introduce() {
        super.introduce()
        println "I am a student at ${school} and I am majoring in ${major}."
    }
}

// Define a Groovy class called "Manager" that extends the "Employee" class
class Manager extends Employee {

    // Define properties of the "Manager" class
    int numberOfEmployees

    // Define a constructor for the "Manager" class
    Manager(String name, int age, String occupation, int employeeId, String department, int numberOfEmployees) {
        super(name, age, occupation, employeeId, department)
        this.numberOfEmployees = numberOfEmployees
    }

    // Override the "introduce" method from the "Employee" class
    @Override
    void introduce() {
        super.introduce()
        println "I am a manager and I have ${numberOfEmployees} employees reporting to me."
    }
}

// Create a few instances of the "Person", "Employee", "Student", and "Manager" classes
Person person1 = new Person("John Doe", 30, "Software Engineer")
Employee employee1 = new Employee("Jane Doe", 25, "Accountant", 12345, "Finance")
Student student1 = new Student("Bob Smith", 20, "Student", "University of California, Berkeley", "Computer Science")
Manager manager1 = new Manager("Mary Johnson", 40, "Manager", 23456, "Sales", 10)

// Invoke the "introduce" method on each instance
person1.introduce()
employee1.introduce()
student1.introduce()
manager1.introduce()