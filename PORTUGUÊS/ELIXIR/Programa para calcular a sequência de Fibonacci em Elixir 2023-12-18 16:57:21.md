Claro! Aqui está um código complexo em Elixir:

```elixir
defmodule Fibonacci do
  def calcular(n) when n < 0 do
    IO.puts("Número inválido! Por favor, insira um número maior ou igual a zero.")
  end

  def calcular(n) do
    IO.puts("Calculando o número #{n} da sequência de Fibonacci...")
    resultado = fibonacci(n)
    IO.puts("O número #{n} da sequência de Fibonacci é #{resultado}.")
  end

  defp fibonacci(0), do: 0
  defp fibonacci(1), do: 1
  defp fibonacci(n), do: fibonacci(n - 1) + fibonacci(n - 2)
end

defmodule Main do
  def run do
    IO.puts("Bem-vindo ao programa de cálculo da sequência de Fibonacci!")
    IO.puts("Por favor, insira um número para calcular o respectivo termo na sequência:")

    input = IO.gets("") |> String.trim() |> String.to_integer()

    Fibonacci.calcular(input)

    IO.puts("Deseja calcular outro número da sequência de Fibonacci? (S/N)")

    resposta = IO.gets("") |> String.trim() |> String.upcase()

    if resposta == "S" do
      run()
    else
      IO.puts("Obrigado por utilizar o programa!")
    end
  end
end

Main.run()
```

Neste código em Elixir, temos duas definições de módulos. O primeiro módulo chamado "Fibonacci" é responsável por calcular o número correspondente na sequência de Fibonacci para um dado valor de entrada. Ele utiliza uma função privada chamada "fibonacci" para realizar o cálculo recursivo.

O segundo módulo chamado "Main" é responsável por executar o programa principal. Ele exibe uma mensagem de boas-vindas e solicita ao usuário um número para calcular na sequência de Fibonacci. Em seguida, chama a função "calcular" do módulo "Fibonacci". Após exibir o resultado, o programa pergunta ao usuário se deseja calcular outro número da sequência e continua executando ou encerra a execução. 

Espero que este código complexo em Elixir atenda às suas expectativas!