Claro! Aqui está um código complexo em Elixir que envolve a manipulação de listas e recursão para calcular a soma dos quadrados dos elementos pares dentro de uma lista:

```elixir
defmodule SomaQuadrados do
  def soma_quadrados_pares(lista) do
    soma_quadrados_pares(lista, 0)
  end

  defp soma_quadrados_pares([], soma), do: soma

  defp soma_quadrados_pares([cabeca | cauda], soma) do
    if rem(cabeca, 2) == 0 do
      soma_quadrados_pares(cauda, soma + (cabeca * cabeca))
    else
      soma_quadrados_pares(cauda, soma)
    end
  end
end

# Exemplo de uso
lista = [1, 2, 3, 4, 5, 6, 7, 8, 9]
soma = SomaQuadrados.soma_quadrados_pares(lista)
IO.puts("A soma dos quadrados dos elementos pares da lista é: #{soma}")
```

Neste código, criamos um módulo chamado `SomaQuadrados` com uma função `soma_quadrados_pares` que recebe uma lista como argumento. A função chama uma função privada `soma_quadrados_pares/2`, que também recebe a lista e uma variável `soma` inicializada com zero.

A função `soma_quadrados_pares/2` tem como base a recursão. Primeiro, verificamos se a lista está vazia. Se estiver, retornamos a soma acumulada até o momento. Caso contrário, verificamos se o elemento da cabeça da lista é par (através da função `rem/2`, que retorna o resto da divisão inteira). Se for par, adicionamos o quadrado desse elemento à soma acumulada e chamamos a função novamente, passando a cauda da lista e a soma atualizada. Se não for par, apenas chamamos a função novamente com a cauda da lista e a soma atual.

No exemplo de uso fornecido, criamos uma lista de números de 1 a 9. Em seguida, chamamos a função `soma_quadrados_pares` do módulo `SomaQuadrados`, passando a lista como argumento. Por fim, imprimimos o resultado utilizando a função `IO.puts`.

Esse código em Elixir calcula a soma dos quadrados dos elementos pares de uma lista, mostrando o uso de recursão e manipulação de listas em Elixir.