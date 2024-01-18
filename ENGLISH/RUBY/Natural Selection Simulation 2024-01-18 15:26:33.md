```
# This code simulates a simplified version of the real world process of natural selection.
# It starts with a population of organisms and specifies the probability that each one will survive and reproduce.
# The more successful organisms pass their traits on to their offspring, and the less successful ones are eliminated.
# Over time, this leads to new populations that are better adapted to their environment.

# Define the population size and the probability of survival and reproduction.
population_size = 100
survival_probability = 0.5
reproduction_probability = 0.2

# Create the population of organisms.
population = []
population_size.times do
  population << {
    survival: rand < survival_probability,
    reproduction: rand < reproduction_probability
  }
end

# Simulate natural selection for a number of generations.
generations = 100
generations.times do
  # Calculate the fitness of each organism.
  fitness = []
  population.each do |organism|
    fitness << organism[:survival] ? 1 : 0
    fitness << organism[:reproduction] ? 1 : 0
  end

  # Select the organisms that will survive and reproduce.
  survivors = []
  reproducers = []
  until survivors.size == population_size / 2
    index = fitness.index(fitness.max)
    survivors << population[index]
    fitness[index] = -1  # So this organism can't be selected again.
  end
  until reproducers.size == population_size / 4
    index = fitness.index(fitness.max)
    reproducers << population[index]
    fitness[index] = -1  # So this organism can't be selected again.
  end

  # Create a new population.
  new_population = []
  reproducers.each do |reproducer|
    new_population << {
      survival: reproducer[:survival],
      reproduction: reproducer[:reproduction]
    }
    new_population << {
      survival: rand < survival_probability,
      reproduction: rand < reproduction_probability
    }
  end

  # Replace the old population with the new one.
  population = new_population
end

# Print the final population.
puts population.inspect
```

This code simulates the process of natural selection over a number of generations.

The population starts with a certain number of organisms, each with a certain probability of survival and reproduction.

The code then simulates the process of natural selection by selecting the organisms that will survive and reproduce based on their fitness.

The fitness of an organism is determined by its probability of survival and reproduction.

The code then creates a new population by selecting the offspring of the surviving and reproducing organisms.

The code then repeats this process for a number of generations, and prints the final population.

The code is complex because it simulates a complex process, and it uses a number of different data structures and algorithms.

The code is also complex because it is written in a functional programming style, which is not common in Ruby.