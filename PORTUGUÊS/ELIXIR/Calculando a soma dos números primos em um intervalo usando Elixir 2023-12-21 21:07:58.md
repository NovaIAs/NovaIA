Claro! Aqui está um código em Elixir que calcula a soma de todos os números primos em um determinado intervalo:

```elixir
defmodule SomaNumerosPrimos do
  def soma_primos(inicio, fim) do
    Enum.map(inicio..fim, &(&1))
    |> Enum.filter(&is_primo/1)
    |> Enum.reduce(0, &(&1 + &2))
  end

  defp is_primo(n) when n <= 1 do
    false
  end

  defp is_primo(n) do
    Enum.all?(2..div(n, 2), fn i -> rem(n, i) != 0 end)
  end
end

inicio = 1
fim = 1000

IO.puts "Calculando a soma dos números primos entre #{inicio} e #{fim}..."
IO.puts "Resultado: #{SomaNumerosPrimos.soma_primos(inicio, fim)}"
```

Neste código, definimos um módulo chamado `SomaNumerosPrimos`. A função `soma_primos` recebe dois parâmetros `inicio` e `fim`, que representam o intervalo de números que queremos calcular a soma dos números primos.

No corpo da função, usamos a função `Enum.map` para criar uma lista contendo todos os números do intervalo. Em seguida, utilizamos a função `Enum.filter` em conjunto com a função `is_primo` (definida posteriormente) para filtrar apenas os números primos da lista.

Finalmente, usamos a função `Enum.reduce` para somar todos os números primos da lista filtrada e retornar o resultado.

A função `is_primo` é uma função auxiliar que verifica se um número é primo ou não. Primeiro, tratamos os casos em que o número é 0 ou 1, retornando `false`. Caso contrário, verificamos se o número é divisível por algum número entre 2 e a metade do seu valor utilizando a função `Enum.all?`. Se encontrarmos algum divisor, retornamos `false`, caso contrário, retornamos `true`.

No final do código, definimos as variáveis `inicio` e `fim` com os valores desejados e exibimos o resultado da soma dos números primos no intervalo escolhido utilizando a função `IO.puts`.

Espero que este código atenda aos seus requisitos!