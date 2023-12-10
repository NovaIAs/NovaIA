Claro! Aqui está um exemplo de código em Elixir que envolve diversas funcionalidades e é bastante complexo:

```elixir
defmodule Contagem do
  def contagem_regressiva(n) do
    Enum.reverse(1..n)
    |> Enum.map(fn x ->
      IO.puts "Faltam #{x} segundos"
      :timer.sleep(1000)
    end)
    IO.puts "Lançamento!"
  end

  def fib(n) do
    case n do
      0 -> 0
      1 -> 1
      _ -> fib(n-1) + fib(n-2)
    end
  end

  defp eh_primo?(n) do
    Enum.all?(2..(div(n, 2)), fn x -> rem(n, x) != 0 end)
  end

  def soma_primos(n) do
    Enum.reduce(2..n, 0, fn x, acc ->
      if eh_primo?(x) do
        acc + x
      else
        acc
      end
    end)
  end
end

defmodule MyApp do
  def run() do
    Contagem.contagem_regressiva(5)
    IO.puts "O 10º número de Fibonacci é #{Contagem.fib(10)}"
    IO.puts "A soma dos números primos até 100 é #{Contagem.soma_primos(100)}"
  end
end

MyApp.run()
```

Este código em Elixir apresenta duas definições de módulos: `Contagem` e `MyApp`. O módulo `Contagem` contém três funções principais:

1. `contagem_regressiva/1`: Esta função recebe um número inteiro `n` e faz uma contagem regressiva a partir desse número, imprimindo a quantidade de segundos restantes a cada segundo. No final, exibe "Lançamento!".

2. `fib/1`: Esta função calcula o enésimo número de Fibonacci, onde `n` é um número inteiro não negativo. Utiliza uma recursão simples para calcular o valor desejado.

3. `soma_primos/1`: Esta função calcula a soma de todos os números primos até um número inteiro `n`. Utiliza a função auxiliar `eh_primo?/1` para verificar se um número é primo ou não, e em seguida, realiza a redução dos números primos utilizando a função `Enum.reduce/3`.

O módulo `MyApp` é responsável por executar o código principal. Chama a função `contagem_regressiva/1` com o argumento 5, exibe o décimo número de Fibonacci e a soma dos números primos até 100.

Espero que este código complexo em Elixir atenda às suas expectativas! Se você tiver mais alguma solicitação, por favor, me avise.