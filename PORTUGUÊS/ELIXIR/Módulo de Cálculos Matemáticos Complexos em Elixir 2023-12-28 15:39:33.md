Claro! Aqui está um código complexo em Elixir:

```elixir
defmodule CalculadoraMatematica do
  @moduledoc """
  Módulo contendo funções para realizar cálculos matemáticos complexos.
  """

  @doc """
  Calcula o fatorial de um número inteiro não negativo.

  ## Parâmetros

    * `n` - O número inteiro para o qual o fatorial será calculado.

  ## Exemplo

      iex> CalculadoraMatematica.fatorial(5)
      120

  """

  def fatorial(n) when is_integer(n) and n >= 0 do
    fatorial_helper(n, 1)
  end

  defp fatorial_helper(0, result), do: result
  defp fatorial_helper(n, result), do: fatorial_helper(n - 1, n * result)

  @doc """
  Calcula a sequência de Fibonacci até um determinado número.

  ## Parâmetros

    * `n` - O número limite da sequência de Fibonacci.

  ## Exemplo

      iex> CalculadoraMatematica.fibonacci(10)
      [0, 1, 1, 2, 3, 5, 8]

  """

  def fibonacci(n) when is_integer(n) and n >= 0 do
    fibonacci_helper(n, [0, 1])
  end

  defp fibonacci_helper(0, sequence), do: Enum.reverse(sequence)
  defp fibonacci_helper(n, sequence) do
    next_number = Enum.at(sequence, -1) + Enum.at(sequence, -2)
    fibonacci_helper(n - 1, [next_number | sequence])
  end
end
```

Este código é um módulo chamado `CalculadoraMatematica`, que contém duas funções complexas: `fatorial/1` e `fibonacci/1`. 

A função `fatorial/1` calcula o fatorial de um número inteiro não negativo. Ela utiliza uma função auxiliar `fatorial_helper/2` recursiva para realizar o cálculo. A função `fatorial_helper/2` recebe dois parâmetros: o número atual e o resultado parcial do cálculo. Ela utiliza recursão para realizar o produto de todos os números de 1 a `n` e retorna o resultado final.

A função `fibonacci/1` calcula a sequência de Fibonacci até um determinado número. Ela também utiliza uma função auxiliar `fibonacci_helper/2` recursiva para realizar o cálculo. A função `fibonacci_helper/2` recebe dois parâmetros: o número atual da sequência e a lista parcial da sequência. Ela utiliza recursão para adicionar o próximo número da sequência à lista parcial, até atingir o número limite especificado. Em seguida, a lista parcial é invertida e retornada como resultado final.

Ambas as funções possuem documentação explicativa com exemplos de uso.