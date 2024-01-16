```ruby
# Define a class called 'Alien' with various properties and methods
class Alien:
  # Initialize the Alien object with attributes like name, planet, and language
  def initialize(name, planet, language)
    @name = name
    @planet = planet
    @language = language
  end

  # Define a method 'speak' that prints the alien's name and language
  def speak
    puts "My name is #{@name}. I speak #{@language}."
  end

  # Define a method 'visit_earth' that prints a message about the alien visiting Earth
  def visit_earth
    puts "I, #{@name}, am visiting Earth from #{@planet}."
  end

  # Define a method 'learn_new_language' that takes a new language as an argument and updates the alien's language attribute
  def learn_new_language(new_language)
    @language = new_language
    puts "I, #{@name}, have learned to speak #{new_language}."
  end
end

# Create an instance of the Alien class called 'Zork' with specific properties
zork = Alien.new("Zork", "Zortex", "Zorgish")

# Call the 'speak' method on the 'Zork' object
zork.speak

# Call the 'visit_earth' method on the 'Zork' object
zork.visit_earth

# Call the 'learn_new_language' method on the 'Zork' object to teach it a new language
zork.learn_new_language("English")
```

Explanation:

1. We define a class called 'Alien' with three instance variables: '@name', '@planet', and '@language'.

2. Inside the 'Alien' class, we define various methods:
   - 'initialize': This method initializes the alien object with its name, planet, and language.
   - 'speak': This method prints the alien's name and the language it speaks.
   - 'visit_earth': This method prints a message indicating that the alien is visiting Earth.
   - 'learn_new_language': This method allows the alien to learn a new language by updating its '@language' attribute.

3. We create an instance of the 'Alien' class called 'zork' with specific properties.

4. We call the 'speak' method on the 'zork' object to make it speak.

5. We call the 'visit_earth' method on the 'zork' object to make it visit Earth.

6. Finally, we call the 'learn_new_language' method on the 'zork' object to teach it a new language.

This code demonstrates the use of classes, objects, methods, and instance variables in Ruby. It creates an alien character with various attributes and allows the alien to speak, visit Earth, and learn new languages.