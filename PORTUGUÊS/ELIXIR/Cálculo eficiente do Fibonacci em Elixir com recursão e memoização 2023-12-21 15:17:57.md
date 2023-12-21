Claro! Aqui está um exemplo de código complexo em Elixir:

```elixir
defmodule Fibonacci do
  def calcular(n) do
    calcular_fibonacci(n, %{}, [0, 1])
  end

  defp calcular_fibonacci(0, memo, _) do
    Map.get(memo, 0)
  end
  
  defp calcular_fibonacci(n, memo, [a, b]) do
    if Map.has_key?(memo, n) do
      Map.get(memo, n)
    else
      resultado = a + b
      memo = Map.put(memo, n, resultado)
      calcular_fibonacci(n - 1, memo, [resultado, a])
    end
  end
end

defmodule Main do
  def run do
    IO.puts("Digite um número para calcular o Fibonacci:")
    numero = IO.gets() |> String.trim() |> String.to_integer()
  
    resultado = Fibonacci.calcular(numero)
  
    IO.puts("O Fibonacci de #{numero} é #{resultado}")
  end
end

Main.run()
```

Neste código, criamos uma função `calcular/1` no módulo `Fibonacci` que recebe um número `n` como parâmetro. Essa função chama a função `calcular_fibonacci/3` passando o número `n`, um mapa vazio para memoização e uma lista inicial `[0, 1]` que representa os dois primeiros números da sequência de Fibonacci.

A função `calcular_fibonacci/3` é responsável por calcular o valor do Fibonacci de um determinado número `n`. Primeiro, verifica se o valor já está presente no mapa memoizado. Se estiver, retorna o valor correspondente. Caso contrário, calcula o próximo número da sequência somando os dois últimos números da lista `[a, b]`, atualiza o mapa memoizado com o novo valor e chama recursivamente a função decrementando o valor de `n` e atualizando a lista `[resultado, a]`.

No módulo `Main`, temos a função `run/0` que solicita ao usuário um número para calcular o Fibonacci e chama a função `calcular/1` do módulo `Fibonacci`. Em seguida, imprime o resultado na tela.

Esse código utiliza recursão e memoização para calcular o Fibonacci de forma eficiente. A memoização evita recálculos desnecessários e melhora a performance do algoritmo.

Espero que esse código atenda às suas expectativas!