```php
<?php

// Define a class called "Employee" with the following properties:
// - id: integer (unique identifier)
// - name: string
// - department: string
// - salary: float
class Employee {
    private $id;
    private $name;
    private $department;
    private $salary;

    // Constructor to initialize the properties
    public function __construct($id, $name, $department, $salary) {
        $this->id = $id;
        $this->name = $name;
        $this->department = $department;
        $this->salary = $salary;
    }

    // Getter methods to access the private properties
    public function getId() {
        return $this->id;
    }

    public function getName() {
        return $this->name;
    }

    public function getDepartment() {
        return $this->department;
    }

    public function getSalary() {
        return $this->salary;
    }

    // Setter methods to update the private properties
    public function setName($name) {
        $this->name = $name;
    }

    public function setDepartment($department) {
        $this->department = $department;
    }

    public function setSalary($salary) {
        $this->salary = $salary;
    }
}

// Create an instance of the "Employee" class with the following values:
// - id: 1
// - name: "John Doe"
// - department: "Sales"
// - salary: 50000
$employee = new Employee(1, "John Doe", "Sales", 50000);

// Create an array of "Employee" objects
$employees = array();
array_push($employees, $employee); // Add the created employee to the array

// Create a function to calculate the total salary of all employees
function calculateTotalSalary($employees) {
    $totalSalary = 0;
    foreach ($employees as $employee) {
        $totalSalary += $employee->getSalary();
    }

    return $totalSalary;
}

// Print the total salary of all employees
echo "Total Salary: $" . calculateTotalSalary($employees);

// Use a "foreach" loop to iterate over the "employees" array and print the details of each employee
echo "\nEmployee Details:\n";
foreach ($employees as $employee) {
    echo "ID: " . $employee->getId() . "\n";
    echo "Name: " . $employee->getName() . "\n";
    echo "Department: " . $employee->getDepartment() . "\n";
    echo "Salary: $" . $employee->getSalary() . "\n\n";
}

?>
```

This code demonstrates several programming concepts and techniques in PHP:

1. **Class Definition and Object Creation:**
   - It defines a class called "Employee" with four properties: "id," "name," "department," and "salary."
   - It creates an instance of the "Employee" class with specific values and assigns it to the variable "$employee."

2. **Array Manipulation:**
   - It creates an array called "employees" to store multiple "Employee" objects.
   - It adds the created "employee" object to the "employees" array using the "array_push()" function.

3. **Function Definition and Execution:**
   - It defines a function called "calculateTotalSalary" to calculate the total salary of all employees.
   - It calls the "calculateTotalSalary" function with the "employees" array as an argument and prints the result.

4. **Object Iteration and Property Access:**
   - It uses a "foreach" loop to iterate over the "employees" array and access the properties of each "Employee" object.
   - It prints the details of each employee, including their ID, name, department, and salary.

5. **Output Formatting:**
   - It uses "echo" statements to display the total salary and the details of each employee in a readable format.

Overall, this code demonstrates how to create and manipulate objects, handle arrays, define and execute functions, and use loops for object iteration in PHP. It provides a comprehensive example of multiple programming concepts within a single script.