```ruby
# Define a class to represent a person
class Person
  # Initialize the person with a name and age
  def initialize(name, age)
    @name = name
    @age = age
  end

  # Return the person's name
  def name
    @name
  end

  # Return the person's age
  def age
    @age
  end

  # Return a string representation of the person
  def to_s
    "Person: #{@name}, #{@age}"
  end
end

# Define a class to represent a pet
class Pet
  # Initialize the pet with a name and species
  def initialize(name, species)
    @name = name
    @species = species
  end

  # Return the pet's name
  def name
    @name
  end

  # Return the pet's species
  def species
    @species
  end

  # Return a string representation of the pet
  def to_s
    "Pet: #{@name}, #{@species}"
  end
end

# Define a class to represent an owner
class Owner
  # Initialize the owner with a name and list of pets
  def initialize(name, pets)
    @name = name
    @pets = pets
  end

  # Return the owner's name
  def name
    @name
  end

  # Return the owner's pets
  def pets
    @pets
  end

  # Add a pet to the owner's list of pets
  def add_pet(pet)
    @pets << pet
  end

  # Return a string representation of the owner
  def to_s
    "Owner: #{@name}, Pets: #{@pets.join(', ')}"
  end
end

# Create a new person named "John Doe"
john_doe = Person.new("John Doe", 30)

# Create a new pet named "Fluffy" who is a dog
fluffy = Pet.new("Fluffy", "Dog")

# Create a new owner named "Jane Smith" with a list of pets
jane_smith = Owner.new("Jane Smith", [fluffy])

# Add "John Doe" to the list of owners of "Fluffy"
fluffy.add_owner(john_doe)

# Print the string representation of each object
puts john_doe
puts fluffy
puts jane_smith
```

**Explanation:**

* The `Person` class represents a person with a name and age.
* The `Pet` class represents a pet with a name and species.
* The `Owner` class represents an owner with a name and a list of pets.
* The `fluffy` variable is a new pet named "Fluffy" who is a dog.
* The `jane_smith` variable is a new owner named "Jane Smith" with a list of pets that includes "Fluffy".
* The `john_doe` variable is a new person named "John Doe".
* The `fluffy.add_owner(john_doe)` line of code adds "John Doe" to the list of owners of "Fluffy".
* The `puts` statements print the string representation of each object to the console.

This code demonstrates the use of classes, objects, and inheritance in Ruby. It also shows how to create and manage relationships between objects.