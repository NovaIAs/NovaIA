```ruby
# Define a class called "Person" with attributes "name", "age", and "gender"
class Person
  attr_accessor :name, :age, :gender

  # Constructor method to initialize the attributes
  def initialize(name, age, gender)
    @name = name
    @age = age
    @gender = gender
  end

  # Method to return a string representation of the person
  def to_s
    "Name: #{@name}, Age: #{@age}, Gender: #{@gender}"
  end
end

# Define a class called "Student" that inherits from "Person" and adds a "gpa" attribute
class Student < Person
  attr_accessor :gpa

  # Constructor method to initialize the attributes
  def initialize(name, age, gender, gpa)
    super(name, age, gender)
    @gpa = gpa
  end

  # Method to return a string representation of the student
  def to_s
    super + ", GPA: #{@gpa}"
  end
end

# Define a class called "Employee" that inherits from "Person" and adds a "salary" attribute
class Employee < Person
  attr_accessor :salary

  # Constructor method to initialize the attributes
  def initialize(name, age, gender, salary)
    super(name, age, gender)
    @salary = salary
  end

  # Method to return a string representation of the employee
  def to_s
    super + ", Salary: #{@salary}"
  end
end

# Create an array of people, including both students and employees
people = [
  Person.new("John Doe", 30, "Male"),
  Student.new("Jane Smith", 20, "Female", 3.8),
  Employee.new("Michael Jones", 40, "Male", 100000)
]

# Iterate over the array of people and print their details
people.each do |person|
  puts person
end
```

This code defines three classes: "Person", "Student", and "Employee". The "Person" class is a base class with attributes "name", "age", and "gender". The "Student" class inherits from "Person" and adds a "gpa" attribute. The "Employee" class also inherits from "Person" and adds a "salary" attribute.

The code then creates an array of people, including both students and employees. Finally, it iterates over the array of people and prints their details.

Here's an example of the output of the code:

```
Name: John Doe, Age: 30, Gender: Male
Name: Jane Smith, Age: 20, Gender: Female, GPA: 3.8
Name: Michael Jones, Age: 40, Gender: Male, Salary: 100000
```