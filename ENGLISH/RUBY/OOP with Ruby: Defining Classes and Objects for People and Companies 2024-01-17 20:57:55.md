```ruby
# This code defines a class called "Person" with attributes for name, age, and occupation.
class Person
  # The constructor method for the Person class.
  # It takes three parameters: name, age, and occupation.
  def initialize(name, age, occupation)
    # The `@` symbol is used to indicate instance variables,
    # which are associated with a specific object.
    @name = name
    @age = age
    @occupation = occupation
  end

  # This method returns the person's name.
  def name
    @name
  end

  # This method returns the person's age.
  def age
    @age
  end

  # This method returns the person's occupation.
  def occupation
    @occupation
  end

  # This method returns a string with the person's details.
  def to_s
    "Name: #{@name}, Age: #{@age}, Occupation: #{@occupation}"
  end
end

# This code defines a class called "Company" with attributes for name, address, and employees.
class Company
  # The constructor method for the Company class.
  # It takes two parameters: name and address.
  def initialize(name, address)
    @name = name
    @address = address
    @employees = []  # Initialize an empty array to store employees.
  end

  # This method returns the company's name.
  def name
    @name
  end

  # This method returns the company's address.
  def address
    @address
  end

  # This method adds an employee to the company's list of employees.
  def add_employee(employee)
    @employees << employee
  end

  # This method returns a string with the company's details and a list of its employees.
  def to_s
    "Name: #{@name}, Address: #{@address}\nEmployees:\n#{@employees.join("\n")}"
  end
end

# This code creates two instances of the Person class: "person1" and "person2".
person1 = Person.new("John Smith", 30, "Software Engineer")
person2 = Person.new("Jane Doe", 25, "Marketing Manager")

# This code creates an instance of the Company class called "company1".
company1 = Company.new("Acme Corporation", "123 Main Street")

# This code adds the two Person instances to the Company instance's list of employees.
company1.add_employee(person1)
company1.add_employee(person2)

# This code prints the details of the Company instance, including its name, address, and employees.
puts company1
```

Explanation of the code:

* The `Person` class defines a basic person object with attributes for `name`, `age`, and `occupation`.
* The `Company` class defines a company object with attributes for `name`, `address`, and a list of `employees`.
* The `initialize` methods for both classes initialize the attributes with the values passed in when creating a new object.
* The `to_s` method for each class provides a string representation of the object, which is useful for printing or debugging.
* The `add_employee` method for the `Company` class adds an employee object to the list of employees.
* Instances of the `Person` and `Company` classes are created and manipulated to demonstrate the functionality of the classes.
* The details of the `Company` instance, including its name, address, and employees, are printed to the console.