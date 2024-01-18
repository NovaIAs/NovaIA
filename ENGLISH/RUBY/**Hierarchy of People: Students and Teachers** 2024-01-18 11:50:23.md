```ruby
# Define a class to represent a person
class Person
  # Instance variables to store the person's attributes
  attr_accessor :name, :age, :gender

  # Constructor to initialize the person's attributes
  def initialize(name, age, gender)
    @name = name
    @age = age
    @gender = gender
  end

  # Method to display the person's details
  def display_details
    puts "Name: #{@name}"
    puts "Age: #{@age}"
    puts "Gender: #{@gender}"
  end
end

# Define a class to represent a student
class Student < Person
  # Instance variables to store the student's additional attributes
  attr_accessor :roll_number, :marks

  # Constructor to initialize the student's attributes
  def initialize(name, age, gender, roll_number, marks)
    super(name, age, gender)  # Call the parent class's constructor to initialize the common attributes
    @roll_number = roll_number
    @marks = marks
  end

  # Method to display the student's details
  def display_details
    super  # Call the parent class's method to display the common attributes
    puts "Roll Number: #{@roll_number}"
    puts "Marks: #{@marks}"
  end
end

# Define a class to represent a teacher
class Teacher < Person
  # Instance variables to store the teacher's additional attributes
  attr_accessor :subject, :salary

  # Constructor to initialize the teacher's attributes
  def initialize(name, age, gender, subject, salary)
    super(name, age, gender)  # Call the parent class's constructor to initialize the common attributes
    @subject = subject
    @salary = salary
  end

  # Method to display the teacher's details
  def display_details
    super  # Call the parent class's method to display the common attributes
    puts "Subject: #{@subject}"
    puts "Salary: #{@salary}"
  end
end

# Create an instance of the Student class
student = Student.new("John Doe", 20, "Male", 12345, 90)

# Create an instance of the Teacher class
teacher = Teacher.new("Jane Smith", 30, "Female", "Math", 50000)

# Display the details of the student and the teacher
puts "Student Details:"
student.display_details

puts "Teacher Details:"
teacher.display_details
```

**Explanation:**

1. We define three classes: `Person`, `Student`, and `Teacher`. The `Person` class is the parent class for `Student` and `Teacher`, and it contains the common attributes and methods for all persons.
2. The `Student` and `Teacher` classes inherit from the `Person` class and add additional attributes and methods specific to students and teachers, respectively.
3. We create instances of the `Student` and `Teacher` classes and display their details using the `display_details` method.
4. The `super` keyword is used in the constructors and `display_details` methods of the `Student` and `Teacher` classes to call the corresponding methods in the `Person` class. This allows us to reuse the code from the `Person` class without having to duplicate it in the child classes.