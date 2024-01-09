```ruby
# Define a custom class for creating and manipulating complex objects
class ComplexObject
  # Initialize the object with a hash of properties
  def initialize(properties)
    @properties = properties
  end

  # Access the value of a property by its name
  def [](property_name)
    @properties[property_name]
  end

  # Set the value of a property by its name
  def []=(property_name, value)
    @properties[property_name] = value
  end

  # Add a new property to the object
  def add_property(property_name, value)
    @properties[property_name] = value
  end

  # Remove a property from the object
  def remove_property(property_name)
    @properties.delete(property_name)
  end

  # Return a hash representation of the object
  def to_hash
    @properties
  end

  # Define a method for printing a human-readable representation of the object
  def to_s
    "ComplexObject: #{@properties}"
  end
end

# Create a complex object with a hash of properties
complex_object = ComplexObject.new({ name: "My Complex Object", description: "This is a very complex object." })

# Access the value of a property using the [] operator
puts complex_object[:name] # Output: "My Complex Object"

# Set the value of a property using the []= operator
complex_object[:description] = "This is an even more complex object."

# Add a new property to the object using the add_property method
complex_object.add_property(:created_at, Time.now)

# Remove a property from the object using the remove_property method
complex_object.remove_property(:created_at)

# Print a human-readable representation of the object using the to_s method
puts complex_object.to_s # Output: "ComplexObject: { name: "My Complex Object", description: "This is an even more complex object." }"

# Convert the object to a hash using the to_hash method
hash_representation = complex_object.to_hash

# Iterate over the hash representation to access the properties
hash_representation.each do |key, value|
  puts "#{key}: #{value}"
end
```

This code defines a custom class called `ComplexObject` that allows you to create and manipulate complex objects with multiple properties. The object is initialized with a hash of properties, and you can access and modify these properties using the `[]` and `[]=` operators. You can also add and remove properties using the `add_property` and `remove_property` methods.

The class provides a `to_hash` method that converts the object to a hash representation, and a `to_s` method that prints a human-readable representation of the object.

The code demonstrates the usage of the `ComplexObject` class by creating an object, accessing and modifying its properties, and converting it to a hash representation.