```groovy
// defining the Employee class
class Employee {
    String name
    int age
    double salary
    String position

    // constructor to initialize the object's fields
    Employee(String name, int age, double salary, String position) {
        this.name = name
        this.age = age
        this.salary = salary
        this.position = position
    }

    // method to calculate the bonus based on salary and age
    double calculateBonus() {
        if (age > 50) {
            return salary * 0.15
        } else if (age > 40) {
            return salary * 0.10
        } else {
            return salary * 0.05
        }
    }

    // method to print the employee's information
    void printInfo() {
        println "Name: $name"
        println "Age: $age"
        println "Salary: $salary"
        println "Position: $position"
        println "Bonus: ${calculateBonus()}"
    }
}

// defining the Company class
class Company {
    // list of employees
    List<Employee> employees = []

    // method to add an employee to the list
    void addEmployee(Employee employee) {
        employees << employee
    }

    // method to print information about all employees
    void printAllEmployees() {
        println "========== Company Employees =========="
        for (Employee employee in employees) {
            println "--------------------------------"
            employee.printInfo()
            println "--------------------------------"
        }
    }
}

// creating the company object
Company company = new Company()

// adding employees to the company
company.addEmployee(new Employee("John Doe", 30, 3000.0, "Software Engineer"))
company.addEmployee(new Employee("Jane Smith", 45, 4000.0, "Project Manager"))
company.addEmployee(new Employee("Michael Jones", 55, 5000.0, "CEO"))

// printing information about all employees
company.printAllEmployees()
```

This code defines two classes: Employee and Company. The Employee class represents individual employees with attributes such as name, age, salary, and position. It also includes a method to calculate the employee's bonus based on their age and salary. The Company class represents the company itself and has a list of employees. It includes methods to add employees to the list and to print information about all employees in the company.

The code creates a Company object and adds three Employee objects to it, each with different attributes. Then, it calls the printAllEmployees() method on the Company object to print information about all employees in the company, including their names, ages, salaries, positions, and bonuses.