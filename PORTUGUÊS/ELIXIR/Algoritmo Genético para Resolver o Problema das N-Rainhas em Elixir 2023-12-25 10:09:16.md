Claro! Aqui está um exemplo de código complexo em Elixir, utilizando uma implementação de um algoritmo genético para resolver o problema das N-Rainhas:

```elixir
defmodule GeneticoRainhas do
  defstruct populacao: [], tamanho_populacao: 0, taxa_mutacao: 0.0, tamanho_tabuleiro: 0

  def resolver(tamanho_tabuleiro, tamanho_populacao, taxa_mutacao) do
    populacao = inicializar_populacao(tamanho_tabuleiro, tamanho_populacao)
    populacao = avaliar_populacao(populacao)
    melhor_solucao = Enum.max_by(populacao, &(&1[:aptidao]))

    IO.puts "Melhor solução inicial: " <> inspect(melhor_solucao[:cromossomo])

    loop(tamanho_tabuleiro, populacao, taxa_mutacao)
  end

  defp loop(tamanho_tabuleiro, populacao, taxa_mutacao) do
    melhor_solucao = Enum.max_by(populacao, &(&1[:aptidao]))

    if melhor_solucao[:aptidao] == tamanho_tabuleiro do
      IO.puts "Solução encontrada: " <> inspect(melhor_solucao[:cromossomo])
    else
      nova_populacao = []
      tamanho_elitismo = div(tamanho_tabuleiro, 5)

      {elitismo, populacao} = Enum.split(populacao, tamanho_elitismo)
      nova_populacao = nova_populacao ++ elitismo

      Enum.each(populacao, fn _ ->
        pais = selecionar_pais(populacao)
        filhos = crossover(pais)
        
        nova_populacao = nova_populacao ++ filhos
      end)

      nova_populacao = mutacao(nova_populacao, taxa_mutacao)
      nova_populacao = avaliar_populacao(nova_populacao)

      loop(tamanho_tabuleiro, nova_populacao, taxa_mutacao)
    end
  end

  defp inicializar_populacao(tamanho_tabuleiro, tamanho_populacao) do
    Enum.map(1..tamanho_populacao, fn _ ->
      cromossomo = Enum.shuffle(1..tamanho_tabuleiro)
      %{cromossomo: cromossomo, aptidao: 0}
    end)
  end

  defp selecionar_pais(populacao) do
    Enum.random(populacao, 2)
  end

  defp crossover(pais) do
    {pai1, pai2} = pais

    ponto_corte = Enum.random(1..(length(pai1[:cromossomo]) - 1))
    corte_pai1 = Enum.split(pai1[:cromossomo], ponto_corte)
    corte_pai2 = Enum.split(pai2[:cromossomo], ponto_corte)

    filho1 = Map.put(pai1, :cromossomo, corte_pai1 ++ corte_pai2)
    filho2 = Map.put(pai2, :cromossomo, corte_pai2 ++ corte_pai1)

    [filho1, filho2]
  end

  defp mutacao(populacao, taxa_mutacao) do
    Enum.map(populacao, fn individuo ->
      if :random.uniform() <= taxa_mutacao do
        cromossomo = Enum.shuffle(individuo[:cromossomo])
        Map.put(individuo, :cromossomo, cromossomo)
      else
        individuo
      end
    end)
  end

  defp avaliar_populacao(populacao) do
    Enum.map(populacao, fn individuo ->
      aptidao = calcular_aptidao(individuo[:cromossomo])
      Map.put(individuo, :aptidao, aptidao)
    end)
  end

  defp calcular_aptidao(cromossomo) do
    tamanho_tabuleiro = length(cromossomo)
    colisoes = Enum.count(cromossomo, fn rainha ->
      conflitos(rainha, cromossomo) > 0
    end)

    tamanho_tabuleiro - colisoes
  end

  defp conflitos(rainha, cromossomo) do
    Enum.count(cromossomo, fn outra_rainha ->
      rainha != outra_rainha and abs(rainha - outra_rainha) == abs(cromossomo[rainha - 1] - cromossomo[outra_rainha - 1])
    end)
  end
end

GeneticoRainhas.resolver(8, 100, 0.1)
```

Este código implementa um algoritmo genético para resolver o problema das N-Rainhas. O objetivo é posicionar N rainhas em um tabuleiro NxN, de forma que nenhuma rainha possa atacar outra.

O algoritmo começa inicializando uma população de cromossomos, onde cada cromossomo é uma permutação aleatória das posições das rainhas no tabuleiro. Em seguida, a aptidão de cada indivíduo é calculada, representando a quantidade de colisões entre as rainhas.

O algoritmo então entra em um loop, onde seleciona os pais para o crossover, realiza o crossover, aplica mutação, recalcula a aptidão da nova população e verifica se a solução foi encontrada. Caso não tenha sido encontrada, o loop continua.

O algoritmo utiliza elitismo, mantendo os melhores indivíduos de cada geração na nova população. O crossover é realizado dividindo os cromossomos dos pais em um ponto de corte aleatório e combinando as partes. A mutação ocorre com uma taxa definida, onde um cromossomo pode sofrer uma mutação de posição das rainhas.

Finalmente, quando a solução é encontrada, é exibida a configuração das rainhas no tabuleiro que representa a melhor solução encontrada.