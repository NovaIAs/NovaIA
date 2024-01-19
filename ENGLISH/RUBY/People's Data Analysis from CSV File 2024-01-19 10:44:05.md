```ruby
require 'csv'

# Define a class to represent a single row of data from the CSV file
class Person
  attr_accessor :first_name, :last_name, :age, :city

  def initialize(first_name, last_name, age, city)
    @first_name = first_name
    @last_name = last_name
    @age = age
    @city = city
  end

  def to_s
    "#{@first_name} #{@last_name} is #{@age} years old and lives in #{@city}"
  end
end

# Read the CSV file into an array of Person objects
people = []
CSV.foreach('people.csv', headers: true) do |row|
  people << Person.new(*row)
end

# Group the people by their city
people_by_city = people.group_by(&:city)

# Print the name of each city and the names of the people who live there
people_by_city.each do |city, people|
  puts "#{city}:"
  people.each do |person|
    puts "  #{person}"
  end
  puts
end

# Find the oldest person in each city
oldest_people_by_city = people_by_city.map do |city, people|
  [city, people.max_by(&:age)]
end

# Print the name of each city and the name of the oldest person who lives there
oldest_people_by_city.each do |city, person|
  puts "#{city}'s oldest resident is #{person}"
end
```

This code reads a CSV file containing information about people (first name, last name, age, and city) and performs some operations on the data.

The code first defines a class called `Person` that represents a single row of data from the CSV file. The class has four instance variables: `@first_name`, `@last_name`, `@age`, and `@city`. It also has a `to_s` method that returns a string representation of the person.

The code then reads the CSV file into an array of `Person` objects using the `CSV.foreach` method. The `headers:` option is used to specify that the first row of the CSV file contains the column names.

The code then groups the people by their city using the `group_by` method. This method returns a hash where the keys are the city names and the values are arrays of `Person` objects who live in that city.

The code then iterates over the `people_by_city` hash and prints the name of each city and the names of the people who live there.

The code then finds the oldest person in each city using the `max_by` method. This method returns the object in the array with the maximum value for the given block. In this case, the block is `&:age`, which returns the age of the person.

The code then iterates over the `oldest_people_by_city` array and prints the name of each city and the name of the oldest person who lives there.