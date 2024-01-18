```ruby
# Define a module for managing employee records
module EmployeeManager

  # Define a class to represent an employee
  class Employee

    # Initialize the employee with their name, department, and role
    def initialize(name, department, role)
      @name = name
      @department = department
      @role = role
    end

    # Get the employee's name
    def name
      @name
    end

    # Get the employee's department
    def department
      @department
    end

    # Get the employee's role
    def role
      @role
    end

    # Define a method to calculate the employee's salary based on their role
    def calculate_salary
      case @role
      when "Manager"
        50000
      when "Engineer"
        40000
      when "Intern"
        30000
      else
        raise "Invalid role"
      end
    end

    # Define a method to print the employee's details
    def print_details
      puts "Name: #{@name}"
      puts "Department: #{@department}"
      puts "Role: #{@role}"
      puts "Salary: #{calculate_salary}"
    end
  end

  # Define a class to manage a collection of employees
  class EmployeeCollection

    # Initialize the collection with an array of employees
    def initialize(employees)
      @employees = employees
    end

    # Add an employee to the collection
    def add_employee(employee)
      @employees << employee
    end

    # Remove an employee from the collection
    def remove_employee(employee)
      @employees.delete(employee)
    end

    # Print the details of all employees in the collection
    def print_all_details
      @employees.each do |employee|
        employee.print_details
        puts "\n"
      end
    end

    # Find an employee by their name
    def find_employee_by_name(name)
      @employees.find { |employee| employee.name == name }
    end

    # Sort the employees by their salary
    def sort_by_salary
      @employees.sort_by { |employee| employee.calculate_salary }
    end

    # Get the total salary of all employees in the collection
    def total_salary
      @employees.sum { |employee| employee.calculate_salary }
    end
  end
end

# Create a new instance of the EmployeeManager module
employee_manager = EmployeeManager

# Create some employees
employee1 = employee_manager::Employee.new("John Doe", "Engineering", "Manager")
employee2 = employee_manager::Employee.new("Jane Smith", "Sales", "Engineer")
employee3 = employee_manager::Employee.new("Michael Jones", "Marketing", "Intern")

# Create a collection of employees
employee_collection = employee_manager::EmployeeCollection.new([employee1, employee2, employee3])

# Print the details of all employees
employee_collection.print_all_details

# Find an employee by their name
found_employee = employee_collection.find_employee_by_name("Jane Smith")
puts "Found employee: #{found_employee.name}"

# Sort the employees by their salary
sorted_employees = employee_collection.sort_by_salary
puts "Sorted employees by salary:"
sorted_employees.each do |employee|
  employee.print_details
  puts "\n"
end

# Get the total salary of all employees
total_salary = employee_collection.total_salary
puts "Total salary of all employees: #{total_salary}"
```

This code defines a module called `EmployeeManager` that contains two classes: `Employee` and `EmployeeCollection`. The `Employee` class represents an individual employee with attributes such as name, department, and role. It also has methods to calculate the employee's salary based on their role and to print the employee's details.

The `EmployeeCollection` class manages a collection of employees. It has methods to add and remove employees, print the details of all employees, find an employee by name, sort the employees by salary, and get the total salary of all employees.

The code then creates three instances of the `Employee` class and adds them to an instance of the `EmployeeCollection` class. It then prints the details of all employees, finds an employee by name, sorts the employees by salary, and calculates the total salary of all employees.

This code demonstrates the use of modules, classes, methods, and object-oriented programming in Ruby. It also shows how to manage a collection of objects and perform various operations on them.