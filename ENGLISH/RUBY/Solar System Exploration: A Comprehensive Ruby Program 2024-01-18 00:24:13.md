```ruby
# This Ruby code defines a class called "Planet" that represents a celestial body in our solar system.
class Planet
  # The "name" attribute stores the name of the planet.
  attr_accessor :name

  # The "mass" attribute stores the mass of the planet in kilograms.
  attr_accessor :mass

  # The "diameter" attribute stores the diameter of the planet in kilometers.
  attr_accessor :diameter

  # The "distance_from_sun" attribute stores the average distance between the planet and the Sun in astronomical units (AU).
  attr_accessor :distance_from_sun

  # The "orbital_period" attribute stores the time it takes for the planet to complete one orbit around the Sun in Earth years.
  attr_accessor :orbital_period

  # The "rotation_period" attribute stores the time it takes for the planet to complete one rotation on its axis in Earth days.
  attr_accessor :rotation_period

  # The "moons" attribute stores an array of objects representing the planet's moons.
  attr_accessor :moons

  # The "initialize" method is the constructor for the Planet class.
  # It takes the following parameters:
  #   - name: The name of the planet.
  #   - mass: The mass of the planet in kilograms.
  #   - diameter: The diameter of the planet in kilometers.
  #   - distance_from_sun: The average distance between the planet and the Sun in astronomical units (AU).
  #   - orbital_period: The time it takes for the planet to complete one orbit around the Sun in Earth years.
  #   - rotation_period: The time it takes for the planet to complete one rotation on its axis in Earth days.
  #   - moons: An array of objects representing the planet's moons.
  def initialize(name, mass, diameter, distance_from_sun, orbital_period, rotation_period, moons)
    @name = name
    @mass = mass
    @diameter = diameter
    @distance_from_sun = distance_from_sun
    @orbital_period = orbital_period
    @rotation_period = rotation_period
    @moons = moons
  end

  # The "to_s" method returns a string representation of the Planet object.
  def to_s
    "Planet: #{name}, Mass: #{mass} kg, Diameter: #{diameter} km, Distance from Sun: #{distance_from_sun} AU, Orbital Period: #{orbital_period} Earth years, Rotation Period: #{rotation_period} Earth days, Moons: #{moons.join(', ')}"
  end
end

# This code creates an array called "planets" that contains Planet objects representing the eight planets in our solar system.
planets = [
  Planet.new("Mercury", 3.3011e23, 4879.4, 0.387, 0.241, 58.6, []),
  Planet.new("Venus", 4.8675e24, 12104, 0.723, 0.615, 243, []),
  Planet.new("Earth", 5.972e24, 12742, 1.000, 1.000, 23.9345, [
    "Moon"
  ]),
  Planet.new("Mars", 6.4171e23, 6779, 1.524, 1.881, 24.6229, [
    "Phobos",
    "Deimos"
  ]),
  Planet.new("Jupiter", 1.899e27, 139822, 5.203, 11.862, 9.925, [
    "Io",
    "Europa",
    "Ganymede",
    "Callisto"
  ]),
  Planet.new("Saturn", 5.685e26, 116464, 9.537, 29.457, 10.233, [
    "Titan",
    "Enceladus",
    "Rhea",
    "Iapetus"
  ]),
  Planet.new("Uranus", 8.683e25, 50724, 19.191, 84.011, -17.24, [
    "Miranda",
    "Ariel",
    "Umbriel",
    "Titania",
    "Oberon"
  ]),
  Planet.new("Neptune", 1.024e26, 49244, 30.069, 164.793, 16.11, [
    "Triton",
    "Nereid"
  ])
]

# This code prints the name, mass, and diameter of each planet in the "planets" array.
planets.each do |planet|
  puts "Planet: #{planet.name}, Mass: #{planet.mass} kg, Diameter: #{planet.diameter} km"
end

# This code prints the name and number of moons for each planet in the "planets" array.
planets.each do |planet|
  puts "Planet: #{planet.name}, Moons: #{planet.moons.count}"
end

# This code finds the planet with the largest diameter and prints its name.
largest_planet = planets.max_by { |planet| planet.diameter }
puts "Largest Planet by Diameter: #{largest_planet.name}"

# This code finds the planet with the longest orbital period and prints its name.
longest_orbital_period = planets.max_by { |planet| planet.orbital_period }
puts "Longest Orbital Period: #{longest_orbital_period.name}"

# This code finds the planet with the shortest rotation period and prints its name.
shortest_rotation_period = planets.min_by { |planet| planet.rotation_period }
puts "Shortest Rotation Period: #{shortest_rotation_period.name}"
```

Explanation:

1. We define a class called "Planet" that represents a celestial body with attributes such as name, mass, diameter, distance from the Sun, orbital period, rotation period, and moons.
2. We create an array called "planets" that contains Planet objects for each of the eight planets in our solar system.
3. We print the name, mass, and diameter of each planet using the "each" method.
4. We print the name and number of moons for each planet using the "each" method.
5. We find the planet with the largest diameter using the "max_by" method and print its name.
6. We find the planet with the longest orbital period using the "max_by" method and print its name.
7. We find the planet with the shortest rotation period using the "min_by" method and print its name.