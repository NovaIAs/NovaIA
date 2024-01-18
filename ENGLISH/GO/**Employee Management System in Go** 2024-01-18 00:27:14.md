```go
package main

import (
	"fmt"
	"math"
	"sort"
	"strings"
	"time"
)

func main() {
	// Employee struct with fields for name, salary, and hire date
	type Employee struct {
		name      string
		salary    float64
		hireDate  time.Time
		department string
	}

	// Create a slice of Employee structs to store employee data
	employees := []Employee{
		{name: "John Doe", salary: 50000, hireDate: time.Date(2020, 1, 1), department: "Sales"},
		{name: "Jane Smith", salary: 60000, hireDate: time.Date(2019, 7, 15), department: "Marketing"},
		{name: "Michael Jones", salary: 70000, hireDate: time.Date(2018, 4, 22), department: "Engineering"},
		{name: "Mary Johnson", salary: 80000, hireDate: time.Date(2017, 12, 31), department: "Finance"},
		{name: "Robert Brown", salary: 90000, hireDate: time.Date(2016, 3, 8), department: "Management"},
	}

	// Print the original list of employees
	fmt.Println("Original List of Employees:")
	for _, e := range employees {
		fmt.Printf("%-15s %-10.2f %-15s %-10s\n", e.name, e.salary, e.hireDate.Format("Jan 2, 2006"), e.department)
	}

	// Sort the employees by salary in ascending order
	sort.Slice(employees, func(i, j int) bool {
		return employees[i].salary < employees[j].salary
	})

	// Print the list of employees sorted by salary
	fmt.Println("\nEmployees Sorted by Salary:")
	for _, e := range employees {
		fmt.Printf("%-15s %-10.2f %-15s %-10s\n", e.name, e.salary, e.hireDate.Format("Jan 2, 2006"), e.department)
	}

	// Find the employee with the highest salary
	maxSalary := 0.0
	var highestPaidEmployee Employee
	for _, e := range employees {
		if e.salary > maxSalary {
			maxSalary = e.salary
			highestPaidEmployee = e
		}
	}

	// Print the highest paid employee
	fmt.Println("\nHighest Paid Employee:")
	fmt.Printf("%-15s %-10.2f %-15s %-10s\n", highestPaidEmployee.name, highestPaidEmployee.salary, highestPaidEmployee.hireDate.Format("Jan 2, 2006"), highestPaidEmployee.department)

	// Find the total salary of all employees
	totalSalary := 0.0
	for _, e := range employees {
		totalSalary += e.salary
	}

	// Calculate the average salary
	avgSalary := totalSalary / float64(len(employees))

	// Print the average salary
	fmt.Println("\nAverage Salary:", avgSalary)

	// Group employees by department
	employeesByDept := make(map[string][]Employee)
	for _, e := range employees {
		employeesByDept[e.department] = append(employeesByDept[e.department], e)
	}

	// Print employees grouped by department
	fmt.Println("\nEmployees Grouped by Department:")
	for dept, employees := range employeesByDept {
		fmt.Println(dept)
		for _, e := range employees {
			fmt.Printf("\t%-15s %-10.2f %-15s\n", e.name, e.salary, e.hireDate.Format("Jan 2, 2006"))
		}
	}

	// Find the employee with the longest tenure
	longestTenure := 0 * time.Second
	var longestTenuredEmployee Employee
	for _, e := range employees {
		tenure := time.Since(e.hireDate)
		if tenure > longestTenure {
			longestTenure = tenure
			longestTenuredEmployee = e
		}
	}

	// Print the employee with the longest tenure
	fmt.Println("\nEmployee with the Longest Tenure:")
	fmt.Printf("%-15s %-10.2f %-15s %-10s\n", longestTenuredEmployee.name, longestTenuredEmployee.salary, longestTenuredEmployee.hireDate.Format("Jan 2, 2006"), longestTenuredEmployee.department)

	// Find employees whose names contain a specific substring
	substring := "o"
	employeesWithNameSubstring := []Employee{}
	for _, e := range employees {
		if strings.Contains(e.name, substring) {
			employeesWithNameSubstring = append(employeesWithNameSubstring, e)
		}
	}

	// Print employees whose names contain the substring
	fmt.Println("\nEmployees Whose Names Contain the Substring \"" + substring + "\":")
	for _, e := range employeesWithNameSubstring {
		fmt.Printf("%-15s %-10.2f %-15s %-10s\n", e.name, e.salary, e.hireDate.Format("Jan 2, 2006"), e.department)
	}

	// Calculate the total years of experience of all employees
	totalYearsOfExperience := 0
	for _, e := range employees {
		yearsOfExperience := int(math.Floor(time.Since(e.hireDate).Hours() / (24 * 365)))
		totalYearsOfExperience += yearsOfExperience
	}

	// Print the total years of experience
	fmt.Println("\nTotal Years of Experience of All Employees:", totalYearsOfExperience)
}
```

This code is a comprehensive demonstration of various programming concepts in Go, including:

- **Data Structures**: It uses a slice of structs to store employee data, allowing for easy manipulation and sorting of employees.

- **Sorting**: It sorts the employees by salary using the `sort.Slice` function, making it easy to find the highest and lowest paid employees.

- **Finding Maximum and Minimum Values**: It finds the highest paid employee and the employee with the longest tenure using simple loops and comparisons.

- **Calculating Averages**: It calculates the average salary of all employees using a loop and division.

- **Grouping Data**: It groups employees by department using a map, making it easy to view employees within each department.

- **Searching Data**: It finds employees whose names contain a specific substring using a loop and string comparison.

- **Calculating Totals**: It calculates the total years of experience of all employees using a loop and mathematical operations.

- **Formatting Output**: It uses `fmt.Printf` to format the output in a clear and readable manner.

This code showcases the versatility and power of Go in handling complex data structures, performing various operations on them, and presenting the results in a meaningful way.