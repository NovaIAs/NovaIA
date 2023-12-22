defmodule MeuModulo do
  def main do
    lista_numeros = Enum.to_list(1..100) # Criando uma lista com os números de 1 a 100

    resultado = Enum.reduce(lista_numeros, 0, fn numero, acc ->
      if divisible_por_tres?(numero) do
        acc + numero
      else
        acc
      end
    end)

    IO.puts("A soma dos números divisíveis por três é #{resultado}")
  end

  defp divisible_por_tres?(numero) do
    rem(numero, 3) == 0
  end
end

MeuModulo.main()

Neste código em Elixir, estamos criando um módulo chamado "MeuModulo". Dentro desse módulo, temos uma função chamada "main". 

Na função "main", estamos criando uma lista de números de 1 a 100, utilizando a função "Enum.to_list". 

Em seguida, utilizamos a função "Enum.reduce" para percorrer cada número da lista e realizar uma operação. 

Dentro da função de redução, verificamos se o número é divisível por três, utilizando a função auxiliar "divisible_por_tres?". 

Se o número for divisível por três, somamos o número ao acumulador "acc". Caso contrário, mantemos o acumulador inalterado. 

Ao final da redução, a variável "resultado" conterá a soma de todos os números divisíveis por três. 

Por fim, utilizamos a função "IO.puts" para exibir o resultado na tela.