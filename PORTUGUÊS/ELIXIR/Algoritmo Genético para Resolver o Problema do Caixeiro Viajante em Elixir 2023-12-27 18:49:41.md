Claro! Aqui está um exemplo de um código complexo em Elixir que utiliza algoritmo genético para resolver o problema do caixeiro viajante:

```elixir
defmodule CaixeiroViajante do
  defstruct genes: [], fitness: 0

  defp gera_populacao(tamanho, cidades) do
    Enum.map(1..tamanho, fn _ ->
      %CaixeiroViajante{genes: Enum.shuffle(cidades)}
    end)
  end

  defp distancia_entre(cidade1, cidade2) do
    # Cálculo da distância entre duas cidades (implementação real depende da aplicação)
  end

  defp calcula_fitness(caixeiro) do
    Enum.reduce(caixeiro.genes, {0, nil}, fn cidade, {distancia_total, ultima_cidade} ->
      case ultima_cidade do
        nil ->
          {distancia_total, cidade}
        _ ->
          {distancia_total + distancia_entre(cidade, ultima_cidade), cidade}
      end
    end)
  end

  defp seleciona_pais(populacao, quantidade) do
    populacao
    |> Enum.sort_by(& &1.fitness)
    |> Enum.take(quantidade)
  end

  defp crossover(pai1, pai2) do
    ponto_corte = :rand.uniform(length(pai1.genes))
    genes_filho = Enum.take(pai1.genes, ponto_corte) ++ Enum.reject(pai2.genes, &Enum.member?(Enum.take(pai1.genes, ponto_corte), &1))
    %CaixeiroViajante{genes: genes_filho}
  end

  defp mutacao(caixeiro, taxa_mutacao) do
    Enum.map(caixeiro.genes, fn cidade ->
      if :rand.uniform() < taxa_mutacao do
        Enum.random(Enum.reject(caixeiro.genes, &(&1 == cidade)))
      else
        cidade
      end
    end)
    |> %{caixeiro | genes: &1}
  end

  defp nova_geracao(populacao, taxa_mutacao) do
    pais = seleciona_pais(populacao, div(length(populacao), 2))
    filhos = Enum.map(1..length(pais), fn _ ->
      {pai1, pai2} = Enum.random(pais, 2)
      crossover(pai1, pai2)
      |> mutacao(taxa_mutacao)
    end)
    populacao ++ filhos
  end

  def resolver(cidades, tamanho_populacao \\ 100, taxa_mutacao \\ 0.01, geracoes \\ 100) do
    populacao = gera_populacao(tamanho_populacao, cidades)

    Enum.reduce(1..geracoes, populacao, fn geracao, p ->
      nova_geracao(p, taxa_mutacao)
      |> Enum.map(& %{&1 | fitness: calcula_fitness(&1)})
      |> Enum.sort_by(& &1.fitness)
      |> Enum.take(1)
    end)
    |> hd
    |> IO.inspect(label: "Melhor caixeiro viajante")
  end
end

cidades = [
  %{nome: "A", x: 2, y: 3},
  %{nome: "B", x: 4, y: 8},
  %{nome: "C", x: 1, y: 5},
  %{nome: "D", x: 9, y: 6},
  %{nome: "E", x: 7, y: 2}
]

CaixeiroViajante.resolver(cidades)
```

Neste exemplo, o código utiliza um algoritmo genético para resolver o problema do caixeiro viajante. O objetivo é encontrar a rota mais curta que passe por todas as cidades uma única vez.

O código começa definindo uma estrutura chamada `CaixeiroViajante` que armazena os genes (cidades em ordem) e o valor de fitness (distância total percorrida). Em seguida, são implementadas diversas funções auxiliares.

A função `gera_populacao` cria uma população inicial com um número especificado de indivíduos, onde cada indivíduo é uma rota aleatória que visita todas as cidades.

A função `distancia_entre` calcula a distância entre duas cidades, que deve ser implementada de acordo com a aplicação real.

A função `calcula_fitness` calcula o valor de fitness de um indivíduo, somando as distâncias percorridas entre as cidades em sua rota.

A função `seleciona_pais` seleciona os indivíduos com melhores valores de fitness para serem os pais da próxima geração.

A função `crossover` realiza o cruzamento entre dois pais, criando um filho com parte das cidades de cada pai em ordem.

A função `mutacao` aplica uma mutação nas cidades de um indivíduo com uma determinada taxa de mutação, que pode trocar aleatoriamente uma cidade por outra que ainda não foi visitada.

A função `nova_geracao` cria uma nova geração combinando os pais selecionados com cruzamento e mutação.

Por fim, a função `resolver` executa o algoritmo genético. É possível configurar o tamanho da população, taxa de mutação e número de gerações. O melhor caixeiro viajante encontrado é exibido no final.

Este código é apenas um exemplo de como implementar um algoritmo genético para resolver o problema do caixeiro viajante em Elixir. É importante ressaltar que a eficiência e a qualidade da solução podem variar dependendo da configuração do algoritmo e da implementação da função `distancia_entre`.