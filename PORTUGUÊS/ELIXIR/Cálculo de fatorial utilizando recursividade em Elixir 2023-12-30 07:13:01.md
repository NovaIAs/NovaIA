Claro! Aqui está um código complexo em Elixir que utiliza a recursividade para calcular o fatorial de um número:

```elixir
defmodule Fatorial do
  def calcula_fatorial(n) do
    if n < 0 do
      {:error, "O número deve ser não negativo"}
    else
      {:ok, fatorial(n)}
    end
  end

  defp fatorial(0), do: 1
  defp fatorial(n), do: n * fatorial(n - 1)
end
```

Neste código, criamos um módulo chamado `Fatorial` que possui uma função `calcula_fatorial`. Essa função recebe um número `n` como parâmetro e retorna o fatorial desse número, ou um erro caso o número seja negativo.

Dentro da função `calcula_fatorial`, verificamos se o número é negativo. Se for, retornamos um erro informando que o número deve ser não negativo. Caso contrário, chamamos a função privada `fatorial` para calcular o fatorial.

A função `fatorial` é uma função recursiva. Ela possui dois casos base: se o número for 0, o fatorial é 1; caso contrário, o fatorial é calculado multiplicando o número pelo fatorial do número anterior (n - 1).

Espero que esse código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, estou à disposição.