Claro! Aqui está um código complexo em Elixir que utiliza o conceito de recursão para calcular o fatorial de um número:

```elixir
defmodule Fatorial do
  def calcular_fatorial(n) when n >= 0 do
    calcular_fatorial_helper(n, 1)
  end

  def calcular_fatorial(n) when n < 0 do
    "Não é possível calcular o fatorial de um número negativo"
  end

  defp calcular_fatorial_helper(0, acc), do: acc
  defp calcular_fatorial_helper(n, acc) do
    calcular_fatorial_helper(n - 1, n * acc)
  end
end

IO.puts Fatorial.calcular_fatorial(5)
```

Este código define um módulo chamado `Fatorial` que contém duas funções: `calcular_fatorial/1` e `calcular_fatorial_helper/2`.

A função `calcular_fatorial/1` é a função principal que será chamada pelos usuários. Ela recebe um número `n` como argumento e inicializa a recursão chamando a função `calcular_fatorial_helper/2` com `n` e `1` como parâmetros iniciais.

A função `calcular_fatorial/1` também possui um guarda que verifica se o número `n` é menor que zero. Caso seja, a função retorna a mensagem de erro "Não é possível calcular o fatorial de um número negativo".

A função `calcular_fatorial_helper/2` é uma função auxiliar que recebe dois parâmetros: `n` e `acc`. Ela implementa a lógica da recursão para calcular o fatorial.

Na primeira definição desta função, quando `n` é igual a `0`, a função retorna o acumulador `acc`, que é o resultado final do fatorial.

Na segunda definição, quando `n` é diferente de `0`, a função chama a si mesma com `n - 1` e `n * acc` como argumentos. Isso faz com que a função seja chamada repetidamente, decrementando `n` a cada chamada e acumulando o resultado parcial em `acc`. Essa recursão continua até que `n` seja igual a `0`, quando a primeira definição da função é ativada e o resultado final é retornado.

Por fim, a função `IO.puts` é utilizada para imprimir o resultado do cálculo do fatorial de `5`.