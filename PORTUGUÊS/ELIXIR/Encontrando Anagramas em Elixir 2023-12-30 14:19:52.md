Claro! Aqui está um código complexo em Elixir, que utiliza recursão e manipulação de listas para encontrar e retornar todos os anagramas de uma palavra fornecida:

```elixir
defmodule Anagrama do
  def encontrar_anagramas(palavra) do
    letras = String.graphemes(palavra)
    todas_combinacoes(letras, [])
  end

  defp todas_combinacoes([], prefixo), do: [prefixo]
  defp todas_combinacoes(letras, prefixo) do
    for i <- 0..length(letras)-1 do
      letra = List.delete_at(letras, i)
      todas_combinacoes(letra, [letras[i] | prefixo]) ++
        todas_combinacoes(letras, prefixo)
    end
  end
end

IO.puts("Digite uma palavra:")
palavra = IO.gets() |> String.trim()

anagramas = Anagrama.encontrar_anagramas(palavra)
IO.puts("Os anagramas de #{palavra} são:")
Enum.each(anagramas, &IO.puts/1)
```

Explicação do Código:

1. Começamos definindo um módulo chamado `Anagrama`.
2. Dentro do módulo, temos uma função chamada `encontrar_anagramas`, que recebe uma palavra como parâmetro.
3. Na função `encontrar_anagramas`, começamos convertendo a palavra em uma lista de caracteres chamada `letras`.
4. Em seguida, chamamos a função `todas_combinacoes` passando as `letras` e uma lista vazia como argumentos.
5. A função `todas_combinacoes` é uma função privada que faz recursão.
6. Se a lista `letras` estiver vazia, isso significa que todas as combinações foram encontradas, então retornamos uma lista contendo apenas o `prefixo` atual.
7. Caso contrário, iteramos sobre os índices da lista `letras` usando um `for` loop.
8. Para cada índice `i`, removemos a letra correspondente da lista `letras` usando `List.delete_at` e atribuímos a essa letra a variável `letra`.
9. Chamamos a função `todas_combinacoes` passando a `letra` e uma lista que contém o `letras[i]` mais o `prefixo` atual, concatenado com a chamada recursiva `todas_combinacoes(letras, prefixo)`.
10. Por fim, fora das funções, pedimos ao usuário para digitar uma palavra e armazenamos o valor em `palavra`.
11. Chamamos a função `Anagrama.encontrar_anagramas` passando `palavra` como argumento para encontrar todos os anagramas.
12. Imprimimos todos os anagramas encontrados na tela.