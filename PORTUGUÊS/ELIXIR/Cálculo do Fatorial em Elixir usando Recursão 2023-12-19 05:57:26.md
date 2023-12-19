Claro! Aqui está um exemplo de um código complexo em Elixir que utiliza recursão para calcular o fatorial de um número:

```elixir
defmodule Fatorial do
  def calcular_fatorial(n) when n >= 0 do
    calcular_fatorial(n, 1)
  end

  defp calcular_fatorial(0, result), do: result
  defp calcular_fatorial(n, result) do
    calcular_fatorial(n - 1, n * result)
  end
end

IO.puts("Digite um número para calcular o fatorial:")
numero = IO.gets() |> String.trim() |> String.to_integer()

if is_integer(numero) do
  resultado = Fatorial.calcular_fatorial(numero)
  IO.puts("O fatorial de #{numero} é #{resultado}")
else
  IO.puts("Entrada inválida. Por favor, digite um número inteiro.")
end
```

Neste código, definimos um módulo chamado `Fatorial` que contém duas funções: `calcular_fatorial/1` e `calcular_fatorial/2`. A função `calcular_fatorial/1` é a função pública que os usuários devem chamar para calcular o fatorial de um número. Ela valida se o número é maior ou igual a zero e chama a função `calcular_fatorial/2` passando o número e o valor inicial do resultado (que é 1).

A função `calcular_fatorial/2` é uma função privada que utiliza recursão para calcular o fatorial. Ela possui dois casos base: quando `n` é igual a 0, o resultado é retornado; e quando `n` é diferente de 0, a função chama a si mesma passando `n - 1` e `n * result` como argumentos.

Na parte final do código, solicitamos ao usuário que digite um número para calcular o fatorial. O número é lido da entrada padrão, convertido para inteiro e armazenado na variável `numero`. Em seguida, verificamos se o valor é um número inteiro. Se for, chamamos a função `calcular_fatorial/1` com o número digitado e exibimos o resultado. Caso contrário, exibimos uma mensagem de erro.

Espero que este código atenda às suas expectativas! Lembre-se de que você pode personalizá-lo e modificá-lo de acordo com suas necessidades.