Claro! Aqui está um código complexo em Elixir que utiliza recursão e manipulação de listas para gerar uma sequência de Fibonacci:

```elixir
defmodule Fibonacci do
  def calcular(n) when n <= 0 do
    []
  end

  def calcular(n) do
    calcular(n - 1) ++ [fibonacci(n)]
  end

  defp fibonacci(0), do: 0
  defp fibonacci(1), do: 1
  defp fibonacci(n), do: fibonacci(n - 1) + fibonacci(n - 2)
end

IO.puts("Digite a quantidade de números da sequência de Fibonacci:")
quantidade = String.to_integer(IO.gets(""))

IO.inspect(Fibonacci.calcular(quantidade - 1))
```

Explicação do código:
- Primeiramente, definimos um módulo chamado `Fibonacci`.
- Em seguida, definimos uma função `calcular` que recebe um valor `n` (a quantidade de números da sequência de Fibonacci que queremos gerar).
- A primeira cláusula da função `calcular` verifica se `n` é menor ou igual a 0. Se for, retorna uma lista vazia `[]`.
- A segunda cláusula da função `calcular` é a recursão propriamente dita. Ela chama a função `calcular` passando `n - 1` como argumento e concatena o resultado com o valor de `fibonacci(n)`.
- A função `fibonacci` é uma função privada que implementa a lógica do cálculo da sequência de Fibonacci.
- Ela possui três cláusulas:
  - A primeira cláusula trata o caso base em que `n` é igual a 0 e retorna 0.
  - A segunda cláusula trata o caso base em que `n` é igual a 1 e retorna 1.
  - A terceira cláusula é a recursão propriamente dita. Ela chama a função `fibonacci` passando `n - 1` como argumento e soma o resultado com o valor de `fibonacci(n - 2)`.
- Por fim, o código solicita ao usuário que digite a quantidade de números da sequência de Fibonacci que deseja gerar e imprime o resultado usando a função `IO.inspect`.

Espero que este código complexo atenda às suas expectativas!