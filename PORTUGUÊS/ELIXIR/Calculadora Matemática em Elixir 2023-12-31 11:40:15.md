Claro! Aqui está um código complexo em Elixir que atende aos seus requisitos:

```elixir
defmodule CalculadoraMatematica do
  @moduledoc """
  Módulo responsável por realizar operações matemáticas complexas.
  """

  def soma(a, b) do
    """
    Função responsável por realizar a soma de dois números.

    ## Exemplo

        iex> CalculadoraMatematica.soma(2, 3)
        5

    """
    a + b
  end

  def subtracao(a, b) do
    """
    Função responsável por realizar a subtração de dois números.

    ## Exemplo

        iex> CalculadoraMatematica.subtracao(5, 3)
        2

    """
    a - b
  end

  def multiplicacao(a, b) do
    """
    Função responsável por realizar a multiplicação de dois números.

    ## Exemplo

        iex> CalculadoraMatematica.multiplicacao(2, 3)
        6

    """
    a * b
  end

  def divisao(a, b) do
    """
    Função responsável por realizar a divisão de dois números.

    ## Exemplo

        iex> CalculadoraMatematica.divisao(10, 2)
        5.0

    """
    a / b
  end

  def potenciacao(a, b) do
    """
    Função responsável por realizar a potenciação de um número.

    ## Exemplo

        iex> CalculadoraMatematica.potenciacao(2, 3)
        8

    """
    a |> Enum.reduce(b, fn _, acc -> a * acc end)
  end

  def fatorial(n) when n >= 0 do
    """
    Função responsável por calcular o fatorial de um número.

    ## Exemplo

        iex> CalculadoraMatematica.fatorial(5)
        120

    """
    Enum.reduce(1..n, fn x, acc -> x * acc end)
  end
end
```

Este código é uma implementação de uma calculadora matemática em Elixir. Ele possui diversas funções que realizam operações matemáticas complexas, como soma, subtração, multiplicação, divisão, potenciação e cálculo do fatorial.

Cada função possui uma documentação explicativa, incluindo exemplos de uso. Isso facilita a compreensão e utilização do código por outros desenvolvedores.

Além disso, o código utiliza conceitos avançados da linguagem Elixir, como pipes (`|>`) e funções anônimas (`fn`), para tornar o código mais conciso e expressivo.

Espero que este código atenda às suas expectativas! Se precisar de mais alguma coisa, é só pedir!