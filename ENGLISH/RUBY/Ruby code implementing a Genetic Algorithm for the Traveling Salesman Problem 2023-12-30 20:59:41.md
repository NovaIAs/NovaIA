Certainly! Here's a highly complex and differentiated Ruby code that implements a genetic algorithm to solve the traveling salesman problem (TSP). This code uses a combination of object-oriented programming and functional programming principles to achieve an efficient and scalable solution.

```ruby
# Genetic Algorithm for Traveling Salesman Problem

class City
  attr_accessor :x, :y

  def initialize(x, y)
    @x = x
    @y = y
  end

  def distance_to(city)
    Math.sqrt((city.x - @x) ** 2 + (city.y - @y) ** 2)
  end
end

class Tour
  attr_accessor :cities

  def initialize(cities)
    @cities = cities
    @distance = nil
  end

  def distance
    return @distance unless @distance.nil?

    @distance = 0
    @cities.each_with_index do |city, index|
      next_city = index == @cities.size - 1 ? @cities.first : @cities[index + 1]
      @distance += city.distance_to(next_city)
    end
    @distance
  end

  def self.random_tour(cities)
    Tour.new(cities.shuffle)
  end

  def breed_with(other)
    crossover_point = rand(1..@cities.size - 1)
    new_cities = @cities[0...crossover_point] + other.cities.reject { |city| @cities.include?(city) }
    Tour.new(new_cities)
  end

  def mutate
    index1 = rand(0...@cities.size)
    index2 = rand(0...@cities.size)
    @cities[index1], @cities[index2] = @cities[index2], @cities[index1]
  end
end

class Population
  attr_accessor :tours

  def initialize(tours)
    @tours = tours
  end

  def evolve(generations, mutation_rate)
    generations.times do |gen|
      puts "Generation #{gen + 1}: Best distance - #{best_tour.distance}"
      new_tours = [best_tour]

      until new_tours.size == @tours.size
        parent1 = select_parent
        parent2 = select_parent
        child = parent1.breed_with(parent2)
        child.mutate if rand < mutation_rate
        new_tours << child
      end

      @tours = new_tours
    end

    best_tour
  end

  private

  def best_tour
    @tours.min_by(&:distance)
  end

  def select_parent
    tournament_size = (@tours.size * 0.1).to_i
    tournament_size = 2 if tournament_size < 2
    tournament = @tours.sample(tournament_size)
    tournament.min_by(&:distance)
  end
end

# Main program
def run_tsp(cities, population_size, generations, mutation_rate)
  tours = Array.new(population_size) { Tour.random_tour(cities) }
  population = Population.new(tours)
  best_tour = population.evolve(generations, mutation_rate)

  puts "\nBest Tour:"
  best_tour.cities.each { |city| puts "(#{city.x}, #{city.y})" }
  puts "Total Distance: #{best_tour.distance}"
end

# Example usage
cities = [
  City.new(60, 200),
  City.new(180, 200),
  City.new(80, 180),
  City.new(140, 180),
  City.new(20, 160),
  City.new(100, 160),
  City.new(200, 160),
  City.new(140, 140),
  City.new(40, 120),
  City.new(100, 120),
  City.new(180, 100),
  City.new(60, 80),
  City.new(120, 80),
  City.new(180, 60),
  City.new(20, 40),
  City.new(100, 40),
  City.new(200, 40),
  City.new(20, 20),
  City.new(60, 20),
  City.new(160, 20)
]

run_tsp(cities, 50, 1000, 0.02)
```

Explanation:
1. The code begins by defining a `City` class representing a city with `x` and `y` coordinates. The `distance_to` method calculates the Euclidean distance between two cities.
2. The `Tour` class represents a tour (i.e., a sequence of cities). It calculates the total distance of the tour using the `distance` method. `random_tour` creates a random tour using a shuffled list of cities. `breed_with` performs crossover between two tours, and `mutate` randomly swaps two cities in a tour.
3. The `Population` class manages a population of tours. `evolve` performs the evolution process, which includes selection, crossover, and mutation. The best tour is returned after the specified number of generations.
4. The `run_tsp` function sets up the initial population and calls the `evolve` method.
5. An example usage is provided at the end of the code, where a list of cities is defined, and the TSP is solved by running the `run_tsp` function with specified parameters: population size, number of generations, and mutation rate.

Please note that this code may take a significant amount of time to run due to the complexity of the TSP problem and the genetic algorithm. Adjusting the parameters (population size, generations, and mutation rate) may be necessary to achieve better results within a reasonable time frame.