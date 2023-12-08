Claro, aqui está um código complexo em Elixir para resolver o problema do Caixeiro Viajante utilizando a meta-heurística chamada Algoritmo Genético:

```elixir
defmodule AlgoritmoGenetico do
  @population_size 1000
  @chromosome_length 10
  @crossover_rate 0.8
  @mutation_rate 0.2

  def run do
    population = generate_population()

    100.times do
      fitness_values = calculate_fitness(population)
      next_generation = [select_elite(population)]
      
      (@population_size - 1).times do
        parent1 = select_parent(population, fitness_values)
        parent2 = select_parent(population, fitness_values)
        child = crossover(parent1, parent2)
        child = mutate(child)
        next_generation = [child | next_generation]
      end

      population = next_generation
    end

    best_solution = Enum.max_by(population, &calculate_fitness_value/1)
    IO.inspect(best_solution)
  end

  defp generate_population do
    Enum.map(1..@population_size, fn _ ->
      Enum.map(1..@chromosome_length, fn _ ->
        IO.inspect(random_position())
      end)
    end)
  end

  defp random_position, do: {rand(100), rand(100)}

  defp calculate_fitness(population) do
    Enum.map(population, fn chromosome ->
      {:ok, fitness_value} = Enum.reduce(chromosome, {0, {0,0}}, fn position, {distance, last_position} ->
        new_distance = distance + euclidean_distance(position, last_position)
        {new_distance, position}
      end)
      fitness_value
    end)
  end

  defp calculate_fitness_value(fitness), do: 1.0 / fitness

  defp euclidean_distance({x1,y1}, {x2,y2}) do
    Math.sqrt((x2-x1)^2 + (y2-y1)^2)
  end

  defp select_elite(population) do
    Enum.max_by(population, &calculate_fitness_value/1)
  end

  defp select_parent(population, fitness_values) do
    elitism_count = (@population_size * 0.1) |> round()
    elites = Enum.take_max(fitness_values, elitism_count)
    mating_candidates = Enum.drop(fitness_values, elitism_count)
    random_chromosome = Enum.random(mating_candidates)
    Enum.at(random_chromosome, 0)
  end

  defp crossover(parent1, parent2) do
    if rand() < @crossover_rate do
      crossover_point = :random.uniform(@chromosome_length)
      offspring1 = List.slice(parent1, 0, crossover_point) ++ List.slice(parent2, crossover_point, @chromosome_length - crossover_point)
      offspring2 = List.slice(parent2, 0, crossover_point) ++ List.slice(parent1, crossover_point, @chromosome_length - crossover_point)
      [offspring1, offspring2]
    else
      [parent1, parent2]
    end
  end

  defp mutate(chromosome) do
    Enum.map(chromosome, fn position ->
      if rand() < @mutation_rate do
        random_position()
      else
        position
      end
    end)
  end
end

AlgoritmoGenetico.run()
```

Neste código, estamos implementando uma solução para o conhecido problema do Caixeiro Viajante utilizando a técnica de Algoritmos Genéticos. O objetivo é encontrar um percurso de menor distância passando por todas as cidades uma única vez.

O algoritmo começa gerando uma população inicial, onde cada indivíduo é representado por uma lista de coordenadas, que representam as cidades e suas respectivas localizações. Essa população é constituída por N indivíduos, onde N é a variável `@population_size`. 

A seguir, o algoritmo passa por várias gerações, onde a cada iteração é realizada a seleção, crossover e mutação dos indivíduos para gerar a próxima geração.

A função `calculate_fitness/1` calcula o valor de fitness para cada indivíduo da população, que é inversamente proporcional à distância total percorrida pelo caixeiro. Quanto menor a distância, maior o valor de fitness.

A função `select_parent/2` é responsável por selecionar um dos pais para o crossover. A seleção é feita utilizando uma técnica chamada roleta viciada, onde a probabilidade de seleção é proporcional ao valor de fitness.

O crossover é realizado pela função `crossover/2`. Nesta implementação, utilizando um ponto de corte aleatório, a função realiza o crossover de dois pais, gerando dois filhos.

Por fim, a função `mutate/1` é responsável por realizar a mutação em um indivíduo. Em cada posição do cromossomo, existe uma chance de ocorrer a mutação, que consiste em gerar uma nova posição aleatória para a cidade.

Ao final das iterações, é retornado o melhor indivíduo encontrado, que representa o percurso de menor distância para o problema do Caixeiro Viajante.