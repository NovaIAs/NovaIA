Claro! Aqui está um código complexo em Elixir que utiliza concorrência para calcular os números primos em um intervalo específico. Vou explicar o código linha por linha:

```elixir
defmodule CalculadoraPrimos do
  def start(from, to) do
    Task.async_stream(from..to, &CalculadoraPrimos.is_prime/1)
    |> Enum.filter(& &1)
  end

  def is_prime(number) do
    Enum.all?(2..(div(number, 2)), fn divisor -> rem(number, divisor) != 0 end)
  end
end
```

Neste código, criamos um módulo chamado `CalculadoraPrimos`. A função `start` é a função principal que será chamada para calcular os números primos em um intervalo específico. Ela recebe dois parâmetros: `from` (início do intervalo) e `to` (fim do intervalo).

Dentro da função `start`, utilizamos a função `Task.async_stream` para criar uma tarefa assíncrona para cada número no intervalo especificado. Essas tarefas executam a função `is_prime` passando o número correspondente como argumento. O `async_stream` retorna uma coleção de resultados assíncronos.

Em seguida, utilizamos `Enum.filter` para filtrar apenas os números primos da coleção de resultados. Essa função recebe uma função anônima (`& &1`) como argumento, que verifica se o número é verdadeiro (ou seja, é primo).

A função `is_prime` é responsável por verificar se um número é primo ou não. Ela utiliza a função `Enum.all?`, que verifica se todos os elementos de uma enumeração satisfazem uma determinada condição. Neste caso, verificamos se todos os números de 2 até a metade do número (usando `div(number, 2)`) não são divisíveis pelo número em questão.

Para utilizar esse código, você pode chamar a função `CalculadoraPrimos.start` passando o intervalo desejado. Por exemplo:

```elixir
CalculadoraPrimos.start(1, 100)
```

Isso retornará uma lista com todos os números primos no intervalo de 1 a 100.

Espero que este código atenda às suas expectativas! Se precisar de mais alguma coisa, estou à disposição.