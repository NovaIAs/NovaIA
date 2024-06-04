Voici un exemple de code complexe en Ruby qui implémente un algorithme génétique pour résoudre un problème de sac à dos :

```ruby
require 'random'

# Un chromosome représente une solution possible au problème du sac à dos
class Chromosome
  attr_accessor :genes, :fitness

  def initialize(genes)
    @genes = genes
    @fitness = nil
  end

  # Calcule la valeur totale et le poids total des éléments du chromosome
  def evaluate
    total_value = 0
    total_weight = 0

    @genes.each_with_index do |gene, i|
      total_value += gene * @values[i]
      total_weight += gene * @weights[i]
    end

    @fitness = (total_value / total_weight) unless total_weight.zero?
  end
end

# Algorithme génétique pour résoudre un problème de sac à dos
class GeneticAlgorithm
  attr_accessor :population, :mutation_rate, :crossover_rate

  def initialize(population_size, mutation_rate, crossover_rate, values, weights, max_weight)
    @population_size = population_size
    @mutation_rate = mutation_rate
    @crossover_rate = crossover_rate
    @values = values
    @weights = weights
    @max_weight = max_weight
  end

  # Initialise la population avec des chromosomes aléatoires
  def initialize_population
    @population = []

    @population_size.times do
      genes = []

      # Générez des gènes aléatoires pour chaque élément
      @values.size.times do
        genes << Random.rand(2)
      end

      chromosome = Chromosome.new(genes)
      chromosome.evaluate
      @population << chromosome
    end
  end

  # Évalue la valeur de chaque chromosome dans la population
  def evaluate_population
    @population.each do |chromosome|
      chromosome.evaluate
    end
  end

  # Sélectionne les chromosomes les plus adaptés pour la reproduction
  def select_parents
    # Trier la population par niveau d'adaptation
    @population.sort! { |a, b| b.fitness <=> a.fitness }

    # Sélectionnez les meilleurs parents en fonction du taux de croisement
    parents = []

    (@population_size * @crossover_rate).times do
      parents << @population[Random.rand(@population_size)]
    end

    return parents
  end

  # Reproduit deux chromosomes pour créer un nouvel enfant
  def crossover(parent1, parent2)
    # Choisissez un point de croisement aléatoire
    crossover_point = Random.rand(@values.size)

    # Créez un nouvel enfant avec les gènes des parents
    child_genes = []

    child_genes[0...crossover_point] = parent1.genes[0...crossover_point]
    child_genes[crossover_point...parent1.genes.size] = parent2.genes[crossover_point...parent2.genes.size]

    child = Chromosome.new(child_genes)

    return child
  end

  # Mutation aléatoire d'un chromosome
  def mutate(chromosome)
    # Pour chaque gène, muter avec le taux de mutation
    chromosome.genes.each_index do |i|
      if Random.rand < @mutation_rate
        chromosome.genes[i] = Random.rand(2)
      end
    end
  end

  # Faire évoluer la population vers une meilleure solution
  def evolve
    new_population = []

    # Sélectionner les parents et les reproduire
    parents = select_parents

    while new_population.size < @population_size
      child = crossover(*parents.sample(2))
      mutate(child)
      new_population << child
    end

    @population = new_population
  end

  # Trouver la meilleure solution dans la population
  def get_best_solution
    best_chromosome = @population.max { |a, b| a.fitness <=> b.fitness }
    best_genes = best_chromosome.genes
    best_value = 0
    best_weight = 0

    best_genes.each_with_index do |gene, i|
      best_value += gene * @values[i]
      best_weight += gene * @weights[i]
    end

    return best_value, best_weight
  end

  # Résout le problème du sac à dos à l'aide de l'algorithme génétique
  def solve
    initialize_population
    evaluate_population

    # Faire évoluer la population jusqu'à convergence
    while true
      evolve
      evaluate_population

      # Vérifier si une solution acceptable a été trouvée
      best_value, best_weight = get_best_solution

      if best_weight <= @max_weight
        return best_value, best_genes
      end
    end
  end
end

# Exemple d'utilisation
values = [60, 100, 120]
weights = [10, 20, 30]
max_weight = 50
population_size = 100
mutation_rate = 0.1
crossover_rate = 0.7

ga = GeneticAlgorithm.new(population_size, mutation_rate, crossover_rate, values, weights, max_weight)
best_value, best_genes = ga.solve

puts "Meilleure valeur : #{best_value}"
puts "Meilleurs gènes :"
best_genes.each { |gene| printf "%d ", gene }
puts ""
```

**Explication du code :**

* **Chromosome** : Représente une solution possible au problème du sac à dos. Il contient une liste de gènes, où chaque gène représente un élément (0 pour ne pas inclure, 1 pour inclure).
* **Algorithme génétique** : Implémente un algorithme génétique pour résoudre le problème du sac à dos. Il évolue une population de chromosomes pour trouver la meilleure solution.
* **Valeurs**, **Poids** et **Poids max** : Données du problème du sac à dos.
* **initialize_population** : Crée une population initiale de chromosomes aléatoires.
* **evaluate_population** : Calcule la valeur et le poids de chaque chromosome dans la population.
* **select_parents** : Sélectionne les chromosomes les plus adaptés pour la reproduction.
* **crossover** : Crée un nouvel enfant en croisant les gènes de deux parents.
* **mutate** : Modifie aléatoirement les gènes d'un chromosome.
* **evolve** : Évolue la population vers une meilleure solution en sélectionnant, croisant et mutant les chromosomes.
* **get_best_solution** : Renvoie la meilleure solution dans la population.
* **solve** : Résout le problème du sac à dos en utilisant l'algorithme génétique et renvoie la meilleure valeur et les meilleurs gènes.

Ce code implémente un algorithme génétique pour résoudre le problème du sac à dos, qui peut être utilisé pour optimiser des solutions dans des problèmes complexes où de nombreuses solutions possibles existent.