```ruby
# Define a class called `Person` with attributes `name`, `age`, `gender`, and `occupation`.
class Person
  attr_accessor :name, :age, :gender, :occupation

  # Initialize the `Person` object with the given attributes.
  def initialize(name, age, gender, occupation)
    @name = name
    @age = age
    @gender = gender
    @occupation = occupation
  end

  # Define a method `to_s` to return a string representation of the `Person` object.
  def to_s
    "Person: #{name}, Age: #{age}, Gender: #{gender}, Occupation: #{occupation}"
  end
end

# Define a class called `Employee` as a subclass of `Person`.
class Employee < Person
  attr_accessor :salary, :department, :job_title

  # Initialize the `Employee` object with the given attributes, including those inherited from `Person`.
  def initialize(name, age, gender, occupation, salary, department, job_title)
    super(name, age, gender, occupation)
    @salary = salary
    @department = department
    @job_title = job_title
  end

  # Override the `to_s` method to include additional information specific to `Employee` objects.
  def to_s
    super + ", Salary: #{salary}, Department: #{department}, Job Title: #{job_title}"
  end
end

# Define a class called `Student` as a subclass of `Person`.
class Student < Person
  attr_accessor :school, :major, :grade_level

  # Initialize the `Student` object with the given attributes, including those inherited from `Person`.
  def initialize(name, age, gender, occupation, school, major, grade_level)
    super(name, age, gender, occupation)
    @school = school
    @major = major
    @grade_level = grade_level
  end

  # Override the `to_s` method to include additional information specific to `Student` objects.
  def to_s
    super + ", School: #{school}, Major: #{major}, Grade Level: #{grade_level}"
  end
end

# Create an array to store various `Person`, `Employee`, and `Student` objects.
people = []

# Create and add a `Person` object to the array.
person1 = Person.new("John Doe", 30, "Male", "Software Engineer")
people << person1

# Create and add an `Employee` object to the array.
employee1 = Employee.new("Jane Smith", 40, "Female", "Accountant", 80000, "Accounting", "Senior Accountant")
people << employee1

# Create and add a `Student` object to the array.
student1 = Student.new("Michael Jones", 20, "Male", "Student", "Harvard University", "Computer Science", "Sophomore")
people << student1

# Iterate over the array and print the string representation of each person.
people.each do |person|
  puts person.to_s
end
```
Explanation:

1. We define three classes: `Person`, `Employee`, and `Student`. The `Person` class is the base class, and the `Employee` and `Student` classes inherit from it.

2. Each class has its own attributes and methods. The `Person` class has attributes for name, age, gender, and occupation. The `Employee` class has additional attributes for salary, department, and job title. The `Student` class has additional attributes for school, major, and grade level.

3. Each class has a constructor method (`initialize`) to initialize the object with the given attributes.

4. Each class has a `to_s` method to return a string representation of the object. The `Employee` and `Student` classes override the `to_s` method from the `Person` class to include additional information specific to their respective objects.

5. We create an array called `people` to store various `Person`, `Employee`, and `Student` objects.

6. We create and add a `Person` object, an `Employee` object, and a `Student` object to the `people` array.

7. We iterate over the `people` array and print the string representation of each person using the `to_s` method.

This code demonstrates complex class hierarchies, inheritance, polymorphism, and object-oriented programming concepts in Ruby.