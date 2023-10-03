Claro! Aqui está um exemplo de um código em Elixir que implementa um algoritmo genético para resolver o problema clássico do "Problema da Mochila":

```elixir
defmodule AlgoritmoGenetico do
  def calcular_melhor_solucao(pesos, valores, capacidade_mochila, num_geracoes, tamanho_populacao) do
    populacao = gerar_populacao_inicial(pesos, valores, tamanho_populacao)
    
    Enum.reduce(0..num_geracoes, populacao, fn _, populacao_atual ->
      populacao_selecionada = selecionar_populacao(populacao_atual, capacidade_mochila)
      populacao_mutada = mutar_populacao(populacao_selecionada)
      populacao_cruzada = cruzar_populacao(populacao_mutada)
      populacao_avaliada = avaliar_populacao(populacao_cruzada, pesos, valores, capacidade_mochila)
      populacao_nova = selecionar_melhores_individuos(populacao_avaliada, tamanho_populacao)
      
      populacao_nova
    end)
    
  end

  def gerar_populacao_inicial(pesos, valores, tamanho_populacao) do
    Enum.map(1..tamanho_populacao, fn _ ->
      Enum.map(pesos, fn _ ->
        Enum.random(0..1)
      end)
    end)
  end

  def selecionar_populacao(populacao, capacidade_mochila) do
    Enum.filter(populacao, fn individuo ->
      soma_pesos = Enum.zip(individuo, pesos) |> Enum.map(fn {bit, peso} -> bit * peso end) |> Enum.sum()
      soma_pesos <= capacidade_mochila
    end)
  end

  def mutar_populacao(populacao) do
    Enum.map(populacao, fn individuo ->
      Enum.map(individuo, fn bit ->
        if Math.random() < 0.1 do
          if bit == 0, do: 1, else: 0
        else
          bit
        end
      end)
    end)
  end

  def cruzar_populacao(populacao) do
    Enum.map(populacao, fn individuo ->
      indice_pai = Enum.random(0..length(populacao)-1)
      indice_mae = Enum.random(0..length(populacao)-1)

      Enum.zip(individuo, populacao[indice_pai], populacao[indice_mae])
      |> Enum.map(fn {bit, bit_pai, bit_mae} ->
        if Math.random() < 0.5 do
          bit_pai
        else
          bit_mae
        end
      end)
    end)
  end

  def avaliar_populacao(populacao, pesos, valores, capacidade_mochila) do
    Enum.map(populacao, fn individuo ->
      soma_pesos = Enum.zip(individuo, pesos) |> Enum.map(fn {bit, peso} -> bit * peso end) |> Enum.sum()
      soma_valores = Enum.zip(individuo, valores) |> Enum.map(fn {bit, valor} -> bit * valor end) |> Enum.sum()

      {individuo, soma_pesos, soma_valores}
    end)
  end

  def selecionar_melhores_individuos(populacao, tamanho_populacao) do
    Enum.sort_by(populacao, &elem(&1, 2), &>=/2)
    |> Enum.take(tamanho_populacao)
    |> Enum.map(&elem(&1, 0))
  end
end

# Exemplo de uso
pesos = [3, 5, 2, 1, 4]
valores = [8, 10, 6, 3, 7]
capacidade_mochila = 10
num_geracoes = 50
tamanho_populacao = 100

melhor_solucao = AlgoritmoGenetico.calcular_melhor_solucao(pesos, valores, capacidade_mochila, num_geracoes, tamanho_populacao)
IO.inspect(melhor_solucao)
```

Neste código, estamos utilizando um algoritmo genético para resolver o "Problema da Mochila", que consiste em determinar a melhor combinação de itens para colocar em uma mochila, considerando a capacidade máxima da mochila e os pesos e valores de cada item.

O algoritmo genético é uma técnica inspirada na evolução biológica, onde uma população de indivíduos é evoluída ao longo de várias gerações. Cada indivíduo é representado por uma sequência de bits, onde cada bit representa a presença ou ausência de um item na mochila. A aptidão de cada indivíduo é avaliada com base na soma dos valores dos itens selecionados e a população é evoluída através de operadores genéticos, como seleção, mutação e cruzamento.

A função `calcular_melhor_solucao` é o ponto de entrada do algoritmo. Ela recebe os pesos e valores dos itens, a capacidade da mochila, o número de gerações e o tamanho da população. A função começa gerando uma população inicial aleatória através da função `gerar_populacao_inicial`.

Em seguida, um loop é executado por um número de vezes igual ao número de gerações especificado. Dentro do loop, a população atual é submetida a uma série de operações: seleção, mutação, cruzamento e avaliação. A função `selecionar_melhores_individuos` é responsável por selecionar os melhores indivíduos da população atual para compor a próxima geração.

No final do algoritmo, a melhor solução encontrada é exibida na tela.

Este código em Elixir utiliza recursos como funções de ordem superior, compreensão de listas e funções anônimas para implementar o algoritmo genético de forma clara e concisa. Ele é altamente configurável e pode ser facilmente adaptado para resolver outros problemas de otimização.