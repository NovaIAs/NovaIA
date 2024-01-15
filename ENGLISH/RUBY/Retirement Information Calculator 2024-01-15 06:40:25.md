```ruby
# Define a class called "Person" with attributes for name, age, and occupation
class Person
  attr_accessor :name, :age, :occupation

  # Initialize the Person object with the given name, age, and occupation
  def initialize(name, age, occupation)
    @name = name
    @age = age
    @occupation = occupation
  end

  # Define a method to calculate the person's retirement age based on their current age and occupation
  def retirement_age
    retirement_age = 65
    if @occupation == "Police Officer" || @occupation == "Firefighter"
      retirement_age = 55
    elsif @occupation == "Teacher" || @occupation == "Nurse"
      retirement_age = 60
    end
    retirement_age
  end

  # Define a method to calculate the number of years until the person reaches retirement age
  def years_until_retirement
    retirement_age - @age
  end

  # Define a method to return a string representation of the person's information
  def to_s
    "Name: #{@name}, Age: #{@age}, Occupation: #{@occupation}, Retirement Age: #{retirement_age}, Years Until Retirement: #{years_until_retirement}"
  end
end

# Create an array of Person objects representing different individuals
people = [
  Person.new("John Smith", 35, "Software Engineer"),
  Person.new("Jane Doe", 40, "Doctor"),
  Person.new("Michael Jones", 50, "Police Officer"),
  Person.new("Sarah Miller", 55, "Teacher"),
  Person.new("David Brown", 60, "Firefighter")
]

# Print the information of each person in the array
puts "Retirement Information for Each Person:"
people.each do |person|
  puts person.to_s
end
```

This code creates a class called `Person` with attributes for name, age, and occupation. It then defines methods to calculate the person's retirement age based on their occupation and the number of years until they reach retirement age.

The code then creates an array of `Person` objects representing different individuals and prints the information of each person in the array.

The output of the code would be something like this:

```
Retirement Information for Each Person:
Name: John Smith, Age: 35, Occupation: Software Engineer, Retirement Age: 65, Years Until Retirement: 30
Name: Jane Doe, Age: 40, Occupation: Doctor, Retirement Age: 65, Years Until Retirement: 25
Name: Michael Jones, Age: 50, Occupation: Police Officer, Retirement Age: 55, Years Until Retirement: 5
Name: Sarah Miller, Age: 55, Occupation: Teacher, Retirement Age: 60, Years Until Retirement: 5
Name: David Brown, Age: 60, Occupation: Firefighter, Retirement Age: 55, Years Until Retirement: 0
```