```c++
// **HEADER INCLUDE SECTION**:
#include <iostream>
#include <vector>
#include <map>
#include <algorithm>
#include <functional>
#include <numeric>

// **USING STATEMENT TO SIMPLIFY NAMESPACE ACCESS**:
using namespace std;

// **DEFINE A CUSTOM DATA STRUCTURE**:
struct Employee {
    string name;
    int age;
    double salary;
};

// **DEFINE A CONSTANTS**:
const int COMPANY_SIZE = 100;

// **DEFINE A FUNCTION TO PRINT THE EMPLOYEE INFORMATION**:
void printEmployee(const Employee& employee) {
    cout << "Name: " << employee.name << endl;
    cout << "Age: " << employee.age << endl;
    cout << "Salary: " << employee.salary << endl;
    cout << "------------------------" << endl;
}

// **DEFINE A FUNCTION TO COMPARE EMPLOYEES BY AGE**:
bool compareEmployeesByAge(const Employee& emp1, const Employee& emp2) {
    return emp1.age < emp2.age;
}

// **DEFINE A FUNCTION TO COMPARE EMPLOYEES BY SALARY**:
bool compareEmployeesBySalary(const Employee& emp1, const Employee& emp2) {
    return emp1.salary < emp2.salary;
}

// **DEFINE A MAIN FUNCTION**:
int main() {
    // **CREATE A VECTOR OF EMPLOYEES**:
    vector<Employee> employees;

    // **INITIALIZE THE VECTOR WITH SOME DATA**:
    for (int i = 0; i < COMPANY_SIZE; i++) {
        string name = "Employee" + to_string(i);
        int age = rand() % 65 + 18;
        double salary = 1000 + (rand() % 9000);

        employees.push_back({name, age, salary});
    }

    // **PRINT THE ORIGINAL LIST OF EMPLOYEES**:
    cout << "Original List of Employees:" << endl;
    for_each(employees.begin(), employees.end(), printEmployee);

    // **SORT THE EMPLOYEES BY AGE USING std::sort**:
    sort(employees.begin(), employees.end(), compareEmployeesByAge);

    // **PRINT THE EMPLOYEES SORTED BY AGE**:
    cout << endl;
    cout << "Employees Sorted by Age:" << endl;
    for_each(employees.begin(), employees.end(), printEmployee);

    // **SORT THE EMPLOYEES BY SALARY USING std::stable_sort**:
    stable_sort(employees.begin(), employees.end(), compareEmployeesBySalary);

    // **PRINT THE EMPLOYEES SORTED BY SALARY**:
    cout << endl;
    cout << "Employees Sorted by Salary:" << endl;
    for_each(employees.begin(), employees.end(), printEmployee);

    // **CALCULATE THE TOTAL SALARY OF ALL EMPLOYEES**:
    double totalSalary = accumulate(employees.begin(), employees.end(), 0.0, [](double sum, const Employee& employee) {
        return sum + employee.salary;
    });

    // **PRINT THE TOTAL SALARY**:
    cout << endl;
    cout << "Total Salary of All Employees: $" << totalSalary << endl;

    // **FIND THE EMPLOYEE WITH THE HIGHEST SALARY**:
    auto maxSalaryEmployee = max_element(employees.begin(), employees.end(), compareEmployeesBySalary);

    // **PRINT THE INFORMATION OF THE EMPLOYEE WITH THE HIGHEST SALARY**:
    cout << endl;
    cout << "Employee with the Highest Salary:" << endl;
    printEmployee(*maxSalaryEmployee);

    // **FIND THE EMPLOYEE WITH THE LOWEST SALARY**:
    auto minSalaryEmployee = min_element(employees.begin(), employees.end(), compareEmployeesBySalary);

    // **PRINT THE INFORMATION OF THE EMPLOYEE WITH THE LOWEST SALARY**:
    cout << endl;
    cout << "Employee with the Lowest Salary:" << endl;
    printEmployee(*minSalaryEmployee);

    return 0;
}
```

This C++ code is designed to manage and analyze a comprehensive list of employees within a company. It includes the following features:

1. **Employee Data Structure**: The `Employee` struct defines the basic attributes of an employee, including their name, age, and salary.

2. **Employee List Initialization**: The program initializes a vector of `Employee` objects with random data to simulate a company's employee list.

3. **Sorting Functionality**: The code demonstrates how to sort the employee list by age using `std::sort` and by salary using `std::stable_sort`.

4. **Printing Employee Information**: A `printEmployee` function is defined to neatly display the information of a single employee.

5. **Calculating Total Salary**: The code uses the `std::accumulate` algorithm to calculate the total salary of all employees in the company.

6. **Finding Highest and Lowest Salary**: The `std::max_element` and `std::min_element` functions are employed to identify the employee with the highest salary and the employee with the lowest salary, respectively.

7. **Demonstrating Range-based for Loop and Lambda Expressions**: Range-based for loops and lambda expressions are utilized in various places throughout the program to simplify the code and make it more concise.

This program serves as a comprehensive example of data management and analysis in C++, covering various aspects such as data structures, sorting algorithms, and functional programming techniques.