Claro! Aqui está um exemplo de um código complexo em Elixir que utiliza o conceito de recursão para calcular o fatorial de um número:

```elixir
defmodule Fatorial do
  def calcular_fatorial(n) when n >= 0 do
    calcular_fatorial(n, 1)
  end

  defp calcular_fatorial(0, resultado), do: resultado
  defp calcular_fatorial(n, resultado) do
    calcular_fatorial(n - 1, resultado * n)
  end
end

IO.puts("Digite um número:")
numero = IO.gets() |> String.trim() |> String.to_integer()
fatorial = Fatorial.calcular_fatorial(numero)
IO.puts("O fatorial de #{numero} é #{fatorial}.")
```

Neste exemplo, criamos um módulo chamado `Fatorial` que contém uma função `calcular_fatorial` que utiliza recursão para calcular o fatorial de um número. 

A função `calcular_fatorial` tem dois cláusulas: a primeira é uma função de entrada que chama a função privada `calcular_fatorial/2` com o número fornecido pelo usuário e um valor inicial de 1 para o resultado. A segunda cláusula é a função privada `calcular_fatorial/2` que realiza a recursão.

A função privada `calcular_fatorial/2` tem duas cláusulas: a primeira é a condição de parada da recursão, onde se o número for igual a 0, retornamos o resultado atual. A segunda cláusula é a recursão em si, onde subtraímos 1 do número e multiplicamos pelo resultado atual, passando para a próxima chamada recursiva.

No final do código, solicitamos ao usuário que digite um número, convertendo-o para inteiro. Em seguida, chamamos a função `calcular_fatorial` passando o número fornecido e armazenamos o resultado em uma variável chamada `fatorial`. Por fim, exibimos o resultado na tela.

Esse código calcula o fatorial de um número de forma eficiente usando recursão e é um exemplo de como a linguagem Elixir pode ser utilizada para resolver problemas matemáticos de maneira clara e concisa. Lembre-se de testar o código com diferentes valores para obter os fatoriais correspondentes.