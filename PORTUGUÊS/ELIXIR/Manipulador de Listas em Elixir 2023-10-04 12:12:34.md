Claro! Aqui está um código complexo em Elixir que envolve manipulação de listas, funções de ordem superior e recursão:

```elixir
defmodule ManipuladorLista do
  def filtrar_pares_impares(lista) do
    Enum.partition(lista, &(&1 % 2 == 0))
  end

  def calcular_soma(lista) do
    Enum.reduce(lista, 0, &(&1 + &2))
  end

  def calcular_media(lista) do
    soma = calcular_soma(lista)
    soma / length(lista)
  end

  def transformar_string(lista) do
    Enum.map(lista, &Integer.to_string/1)
  end

  def remover_duplicatas(lista) do
    Enum.uniq(lista)
  end

  def inverter_ordem(lista) do
    Enum.reverse(lista)
  end

  def contar_elementos(lista) do
    length(lista)
  end
end

lista = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

{pares, impares} = ManipuladorLista.filtrar_pares_impares(lista)
IO.puts("Pares: #{inspect(pares)}")
IO.puts("Ímpares: #{inspect(impares)}")

soma = ManipuladorLista.calcular_soma(lista)
IO.puts("Soma: #{soma}")

media = ManipuladorLista.calcular_media(lista)
IO.puts("Média: #{media}")

strings = ManipuladorLista.transformar_string(lista)
IO.puts("Lista como strings: #{inspect(strings)}")

sem_duplicatas = ManipuladorLista.remover_duplicatas(lista)
IO.puts("Lista sem duplicatas: #{inspect(sem_duplicatas)}")

invertida = ManipuladorLista.inverter_ordem(lista)
IO.puts("Lista invertida: #{inspect(invertida)}")

quantidade_elementos = ManipuladorLista.contar_elementos(lista)
IO.puts("Quantidade de elementos: #{quantidade_elementos}")
```

Neste código, temos um módulo chamado `ManipuladorLista` que contém várias funções para manipular e analisar listas. 

A função `filtrar_pares_impares` recebe uma lista de números e retorna duas listas separadas, uma contendo os números pares e outra contendo os números ímpares.

A função `calcular_soma` recebe uma lista de números e retorna a soma de todos os elementos.

A função `calcular_media` recebe uma lista de números e calcula a média dos elementos.

A função `transformar_string` recebe uma lista de números e retorna uma lista com os elementos convertidos para strings.

A função `remover_duplicatas` recebe uma lista de elementos e retorna uma nova lista sem elementos duplicados.

A função `inverter_ordem` recebe uma lista e retorna uma nova lista com os elementos em ordem reversa.

A função `contar_elementos` recebe uma lista e retorna a quantidade de elementos presentes nela.

No código principal, utilizamos um exemplo de lista `[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]` para testar todas as funções do módulo `ManipuladorLista`.

Espero que este código complexo atenda às suas expectativas!