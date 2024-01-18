```groovy
import groovy.json.JsonSlurper
import groovy.transform.CompileStatic

// This is a Groovy script that uses the JsonSlurper class to parse a JSON file and then performs various operations on the parsed data.

// Define a static method called 'parseJsonFile' that takes a file name as a parameter and returns a Map containing the parsed JSON data.
@CompileStatic
static Map parseJsonFile(String fileName) {
    // Create a new instance of the JsonSlurper class.
    JsonSlurper jsonSlurper = new JsonSlurper()

    // Parse the JSON file using the JsonSlurper instance and return the parsed data as a Map.
    return jsonSlurper.parse(new File(fileName))
}

// Define a static method called 'printEmployeeDetails' that takes a Map containing employee data as a parameter and prints the details of each employee.
@CompileStatic
static void printEmployeeDetails(Map employeeData) {
    // Iterate over the 'employees' list in the employee data Map.
    employeeData.employees.each { employee ->
        // Print the employee's name, age, and salary.
        println "Employee Name: ${employee.name}"
        println "Employee Age: ${employee.age}"
        println "Employee Salary: ${employee.salary}"
        println()
    }
}

// Define a static method called 'calculateTotalSalary' that takes a Map containing employee data as a parameter and returns the total salary of all employees.
@CompileStatic
static double calculateTotalSalary(Map employeeData) {
    // Iterate over the 'employees' list in the employee data Map.
    double totalSalary = 0
    employeeData.employees.each { employee ->
        // Add the employee's salary to the total salary.
        totalSalary += employee.salary
    }

    // Return the total salary.
    return totalSalary
}

// Define a static method called 'findEmployeeWithHighestSalary' that takes a Map containing employee data as a parameter and returns the employee with the highest salary.
@CompileStatic
static Map findEmployeeWithHighestSalary(Map employeeData) {
    // Iterate over the 'employees' list in the employee data Map.
    Map employeeWithHighestSalary = employeeData.employees[0]
    employeeData.employees.each { employee ->
        // If the employee's salary is greater than the current highest salary, update the employee with the highest salary.
        if (employee.salary > employeeWithHighestSalary.salary) {
            employeeWithHighestSalary = employee
        }
    }

    // Return the employee with the highest salary.
    return employeeWithHighestSalary
}

// Define a static method called 'findEmployeesWithAgeGreaterThan' that takes a Map containing employee data and an age as parameters and returns a list of employees whose age is greater than the specified age.
@CompileStatic
static List findEmployeesWithAgeGreaterThan(Map employeeData, int age) {
    // Create a list to store the employees whose age is greater than the specified age.
    List employeesWithAgeGreaterThan = []

    // Iterate over the 'employees' list in the employee data Map.
    employeeData.employees.each { employee ->
        // If the employee's age is greater than the specified age, add the employee to the list.
        if (employee.age > age) {
            employeesWithAgeGreaterThan.add(employee)
        }
    }

    // Return the list of employees whose age is greater than the specified age.
    return employeesWithAgeGreaterThan
}

// Define a static method called 'sortEmployeesBySalary' that takes a Map containing employee data as a parameter and returns a list of employees sorted by salary in ascending order.
@CompileStatic
static List sortEmployeesBySalary(Map employeeData) {
    // Create a list to store the employees sorted by salary in ascending order.
    List employeesSortedBySalary = []

    // Sort the employees by salary using the 'sort