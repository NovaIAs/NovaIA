Claro! Aqui está um exemplo de código em Ruby que usa um algoritmo genético para resolver o problema do caixeiro-viajante (TSP) de forma eficiente. O código está comentado para facilitar a compreensão:

```ruby
class Populacao
  attr_accessor :rotas

  def initialize(numero_cidades, tamanho_populacao)
    @rotas = []
    tamanho_populacao.times do
      rota = (1..numero_cidades).to_a.shuffle
      @rotas << rota
    end
  end

  def calcular_distancia(rota, matriz_distancias)
    distancia = 0
    rota.each_with_index do |cidade_atual, index|
      proxima_cidade = rota[(index + 1) % rota.size]
      distancia += matriz_distancias[cidade_atual - 1][proxima_cidade - 1]
    end
    distancia
  end

  def avaliar(matriz_distancias)
    @rotas.each do |rota|
      rota.distancia = calcular_distancia(rota, matriz_distancias)
    end
  end

  def selecionar_pais(torneio_size)
    pais = []
    (1..2).each do |i|
      torneio = @rotas.sample(torneio_size)
      pais << torneio.min_by(&:distancia)
    end
    pais
  end

  def crossover(pais)
    filho = []
    ponto_corte = rand(1..(pais[0].size - 1))
    filho[0..ponto_corte] = pais[0][0..ponto_corte]
    pais[1].each do |cidade|
      filho << cidade unless filho.include?(cidade)
    end
    filho
  end

  def mutacao(filho, taxa_mutacao)
    filho.each_with_index do |cidade, index|
      if rand < taxa_mutacao
        swap_index = rand(filho.size)
        filho[index], filho[swap_index] = filho[swap_index], filho[index]
      end
    end
    filho
  end

  def evoluir(matriz_distancias, taxa_mutacao, torneio_size)
    nova_populacao = []
    @rotas.size.times do
      pais = selecionar_pais(torneio_size)
      filho = crossover(pais)
      filho_mutado = mutacao(filho, taxa_mutacao)
      nova_populacao << filho_mutado
    end
    @rotas = nova_populacao
    avaliar(matriz_distancias)
  end
end

def tsp(numero_cidades, tamanho_populacao, numero_iteracoes, taxa_mutacao, torneio_size, matriz_distancias)
  populacao = Populacao.new(numero_cidades, tamanho_populacao)

  numero_iteracoes.times do |i|
    populacao.evoluir(matriz_distancias, taxa_mutacao, torneio_size)
    melhor_rota = populacao.rotas.min_by(&:distancia)
    puts "Iteração: #{i + 1} | Menor distância: #{melhor_rota.distancia} | Rota: #{melhor_rota}"
  end

  melhor_rota = populacao.rotas.min_by(&:distancia)
  puts "\nMelhor Solução Encontrada:"
  puts "Distância: #{melhor_rota.distancia}"
  puts "Rota: #{melhor_rota}"
end

# Defina o número de cidades
numero_cidades = 10

# Defina o tamanho da população
tamanho_populacao = 100

# Defina o número de iterações
numero_iteracoes = 100

# Defina a taxa de mutação (0.0 - 1.0)
taxa_mutacao = 0.1

# Defina o tamanho do torneio para seleção dos pais
torneio_size = 10

# Defina a matriz de distâncias entre as cidades
matriz_distancias = [
  [0, 29, 20, 21, 16, 31, 100, 12, 4, 31],
  [29, 0, 15, 29, 28, 40, 72, 21, 29, 41],
  [20, 15, 0, 15, 14, 25, 81, 9, 23, 27],
  [21, 29, 15, 0, 4, 12, 92, 12, 25, 13],
  [16, 28, 14, 4, 0, 16, 94, 9, 20, 16],
  [31, 40, 25, 12, 16, 0, 95, 24, 36, 3],
  [100, 72, 81, 92, 94, 95, 0, 90, 101, 99],
  [12, 21, 9, 12, 9, 24, 90, 0, 15, 25],
  [4, 29, 23, 25, 20, 36, 101, 15, 0, 35],
  [31, 41, 27, 13, 16, 3, 99, 25, 35, 0]
]

tsp(numero_cidades, tamanho_populacao, numero_iteracoes, taxa_mutacao, torneio_size, matriz_distancias)
```

Este código implementa um algoritmo genético para resolver o Problema do Caixeiro-Viajante, em que o objetivo é encontrar a rota mais curta que passe por todas as cidades exatamente uma vez e retorne à cidade de origem. O algoritmo gera uma população inicial de rotas aleatórias e, em cada iteração, evolui a população através de seleção, crossover e mutação. O processo é repetido por um número fixo de iterações e, no final, é apresentada a melhor solução encontrada.

Espero que este exemplo atenda às suas expectativas! Se precisar de mais alguma ajuda, estou à disposição.