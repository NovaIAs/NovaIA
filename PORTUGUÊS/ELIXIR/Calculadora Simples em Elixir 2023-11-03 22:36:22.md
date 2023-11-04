Claro! Vou gerar um código complexo em Elixir para você. Aqui está:

```elixir
defmodule Calculadora do
  def soma(a, b) do
    IO.puts "Realizando soma..."
    resultado = a + b
    IO.puts "A soma de #{a} e #{b} é igual a #{resultado}"
    resultado
  end

  def subtracao(a, b) do
    IO.puts "Realizando subtração..."
    resultado = a - b
    IO.puts "A subtração de #{a} e #{b} é igual a #{resultado}"
    resultado
  end

  def multiplicacao(a, b) do
    IO.puts "Realizando multiplicação..."
    resultado = a * b
    IO.puts "A multiplicação de #{a} e #{b} é igual a #{resultado}"
    resultado
  end

  def divisao(a, b) do
    IO.puts "Realizando divisão..."
    resultado = a / b
    IO.puts "A divisão de #{a} por #{b} é igual a #{resultado}"
    resultado
  end
end

defmodule Main do
  def run do
    IO.puts "Bem-vindo à Calculadora"
    IO.puts "Selecione uma operação:"
    IO.puts "1 - Soma"
    IO.puts "2 - Subtração"
    IO.puts "3 - Multiplicação"
    IO.puts "4 - Divisão"
    operacao = IO.gets() |> String.trim() |> String.to_integer()
    case operacao do
      1 ->
        IO.puts "Digite o primeiro número:"
        numero1 = IO.gets() |> String.trim() |> String.to_integer()
        IO.puts "Digite o segundo número:"
        numero2 = IO.gets() |> String.trim() |> String.to_integer()
        Calculadora.soma(numero1, numero2)
      2 ->
        IO.puts "Digite o primeiro número:"
        numero1 = IO.gets() |> String.trim() |> String.to_integer()
        IO.puts "Digite o segundo número:"
        numero2 = IO.gets() |> String.trim() |> String.to_integer()
        Calculadora.subtracao(numero1, numero2)
      3 ->
        IO.puts "Digite o primeiro número:"
        numero1 = IO.gets() |> String.trim() |> String.to_integer()
        IO.puts "Digite o segundo número:"
        numero2 = IO.gets() |> String.trim() |> String.to_integer()
        Calculadora.multiplicacao(numero1, numero2)
      4 ->
        IO.puts "Digite o dividendo:"
        numero1 = IO.gets() |> String.trim() |> String.to_integer()
        IO.puts "Digite o divisor:"
        numero2 = IO.gets() |> String.trim() |> String.to_integer()
        Calculadora.divisao(numero1, numero2)
      _ ->
        IO.puts "Operação inválida"
    end
  end
end

Main.run()
```

Este código é uma calculadora simples em Elixir que permite ao usuário selecionar uma operação matemática (soma, subtração, multiplicação ou divisão) e realizar o cálculo com dois números fornecidos pelo usuário.

O código está estruturado em dois módulos: `Calculadora` e `Main`. O módulo `Calculadora` contém as funções para realizar as operações matemáticas, enquanto o módulo `Main` é responsável pelo fluxo principal do programa, exibindo as opções disponíveis ao usuário e chamando as funções apropriadas da `Calculadora` com base na escolha do usuário.

Ao executar o programa, o usuário é recebido com uma mensagem de boas-vindas e as opções de operação são exibidas. O usuário pode digitar o número correspondente à operação desejada e, em seguida, fornecer os números necessários para realizar o cálculo. O resultado é exibido na tela.

O código utiliza recursos básicos da linguagem Elixir, como definição de módulos, definição de funções, manipulação de strings e conversão de tipos. Ele também faz uso da função `IO.puts` para exibir mensagens na tela e da função `IO.gets` para receber entrada do usuário.

Espero que este código atenda às suas necessidades! Se você tiver mais alguma solicitação, por favor, me avise.