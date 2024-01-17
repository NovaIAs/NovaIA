```elixir
defmodule GeneticAlgorithm do
  defstruct population_size: 100, crossover_rate: 0.8, mutation_rate: 0.1

  def new(population_size, crossover_rate, mutation_rate) do
    %__MODULE__{
      population_size: population_size,
      crossover_rate: crossover_rate,
      mutation_rate: mutation_rate
    }
  end

  def generate_initial_population(self) do
    for _ <- 1..self.population_size, do: generate_random_chromosome()
  end

  defp generate_random_chromosome do
    for _ <- 1..100, do: Enum.random(0..1)
  end

  def select_parents(self, population) do
    population
    |> Enum.sort_by(&fitness/1, :desc)
    |> Enum.take(self.population_size / 2)
  end

  defp fitness(chromosome) do
    Enum.sum(chromosome)
  end

  def crossover(self, parent1, parent2) do
    crossover_point = Enum.random(1..99)

    {
      parent1 |> Enum.take(crossover_point) ++ parent2 |> Enum.drop(crossover_point),
      parent2 |> Enum.take(crossover_point) ++ parent1 |> Enum.drop(crossover_point)
    }
  end

  def mutate(self, chromosome) do
    Enum.map_with_index(chromosome, fn gene, index ->
      if :rand.uniform() < self.mutation_rate, do: 1 - gene, else: gene
    end)
  end

  def evolve(self, population) do
    new_population =
      for _ <- 1..self.population_size do
        parents = self.select_parents(population)
        offspring = self.crossover(parents.hd, parents.tl)
        self.mutate(offspring.hd)
      end

    population ++ new_population
  end

  def run(self, initial_population) do
    generations =
      for generation <- 1..100 do
        population = self.evolve(initial_population)
        best_chromosome = population |> Enum.max_by(&fitness/1)
        IO.puts "Generation #{generation}: #{fitness(best_chromosome)}"
      end

    generations |> Enum.last
  end
end

defmodule Main do
  def main do
    algorithm = GeneticAlgorithm.new(100, 0.8, 0.1)
    initial_population = algorithm.generate_initial_population()
    algorithm.run(initial_population)
  end
end

Main.main()
```

This code implements a simple genetic algorithm in Elixir. The genetic algorithm tries to find the best solution to a problem by simulating the process of natural selection. The algorithm starts with a population of random solutions, and then iteratively selects the best solutions, crosses them over to create new solutions, and mutates the new solutions to create even more new solutions. This process is repeated until a solution is found that is good enough, or until a certain number of iterations have been reached.

The `GeneticAlgorithm` struct defines the parameters of the algorithm, including the population size, the crossover rate, and the mutation rate. The `generate_initial_population` function generates a random population of solutions. The `select_parents` function selects the best solutions from the current population. The `crossover` function crosses over two solutions to create two new solutions. The `mutate` function mutates a solution to create a new solution. The `evolve` function evolves the current population by selecting the best solutions, crossing them over, and mutating them. The `run` function runs the algorithm for a specified number of iterations.

The `Main` module defines the main function, which creates a new genetic algorithm object, generates an initial population, and then runs the algorithm.

This code is a good example of how to use Elixir to implement a complex algorithm. The code is clear and concise, and it is easy to understand how the algorithm works.