Claro! Aqui está um código complexo em Elixir que utiliza a recursão para calcular o fatorial de um número:

```elixir
defmodule Fatorial do
  def calcular_fatorial(n) do
    if n < 0 do
      IO.puts("Não é possível calcular o fatorial de um número negativo.")
    else
      calcular_fatorial_aux(n, 1)
    end
  end

  defp calcular_fatorial_aux(0, resultado) do
    resultado
  end

  defp calcular_fatorial_aux(n, resultado) do
    calcular_fatorial_aux(n - 1, resultado * n)
  end
end

IO.puts("Digite um número para calcular o fatorial: ")
numero = String.to_integer(IO.gets(""))
fatorial = Fatorial.calcular_fatorial(numero)
IO.puts("O fatorial de #{numero} é #{fatorial}.")
```

O código começa definindo um módulo chamado `Fatorial`. Dentro desse módulo, há duas funções principais: `calcular_fatorial/1` e `calcular_fatorial_aux/2`.

A função `calcular_fatorial/1` recebe um número `n` e verifica se ele é menor que zero. Se for, exibe uma mensagem informando que não é possível calcular o fatorial de um número negativo. Caso contrário, chama a função `calcular_fatorial_aux/2` passando `n` e 1 como parâmetros iniciais.

A função `calcular_fatorial_aux/2` é uma função recursiva que calcula o fatorial de um número. Ela possui dois casos base: quando `n` é igual a 0, retorna o resultado atual; caso contrário, chama a si mesma passando `n - 1` e `resultado * n` como novos parâmetros. Essa recursão continua até que `n` seja igual a 0, momento em que o resultado final é retornado.

Em seguida, o código solicita ao usuário que digite um número para calcular o fatorial. O valor digitado é convertido para inteiro e armazenado na variável `numero`. A função `calcular_fatorial/1` é então chamada passando o `numero` como argumento, e o resultado é armazenado na variável `fatorial`. Por fim, o resultado é exibido na tela.

Esse código em Elixir utiliza a recursão para calcular o fatorial de um número de forma eficiente e demonstra a estrutura básica de um programa Elixir.