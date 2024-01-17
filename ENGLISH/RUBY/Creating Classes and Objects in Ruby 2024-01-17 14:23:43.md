```ruby
# Define a class called "Person"
class Person
  # Initialize the person with a name and age
  def initialize(name, age)
    @name = name
    @age = age
  end

  # Define a method to return the person's name
  def name
    @name
  end

  # Define a method to return the person's age
  def age
    @age
  end
end

# Define a class called "Student" that inherits from "Person"
class Student < Person
  # Initialize the student with a name, age, and grade
  def initialize(name, age, grade)
    # Call the parent class's initialize method
    super(name, age)

    @grade = grade
  end

  # Define a method to return the student's grade
  def grade
    @grade
  end
end

# Define a class called "Teacher" that inherits from "Person"
class Teacher < Person
  # Initialize the teacher with a name, age, and subject
  def initialize(name, age, subject)
    # Call the parent class's initialize method
    super(name, age)

    @subject = subject
  end

  # Define a method to return the teacher's subject
  def subject
    @subject
  end
end

# Create a new instance of the "Person" class
person = Person.new("John Doe", 30)

# Create a new instance of the "Student" class
student = Student.new("Jane Doe", 18, "A")

# Create a new instance of the "Teacher" class
teacher = Teacher.new("John Smith", 40, "Math")

# Print the person's name, age, and grade
puts "#{person.name} is #{person.age} years old."

# Print the student's name, age, and grade
puts "#{student.name} is #{student.age} years old and has a grade of #{student.grade}."

# Print the teacher's name, age, and subject
puts "#{teacher.name} is #{teacher.age} years old and teaches #{teacher.subject}."
```

This code defines three classes: "Person", "Student", and "Teacher". The "Person" class is the parent class, and the "Student" and "Teacher" classes are child classes that inherit from the "Person" class.

The "Person" class has a constructor that takes two parameters: `name` and `age`. It also has two methods: `name` and `age`, which return the person's name and age, respectively.

The "Student" class inherits from the "Person" class and has a constructor that takes three parameters: `name`, `age`, and `grade`. It also overrides the `age` method to return the student's grade.

The "Teacher" class inherits from the "Person" class and has a constructor that takes three parameters: `name`, `age`, and `subject`. It also overrides the `age` method to return the teacher's subject.

The code then creates three instances of these classes: a "Person" instance, a "Student" instance, and a "Teacher" instance. It then prints the name, age, and grade of the "Student" instance, and the name, age, and subject of the "Teacher" instance.