Claro! Aqui está um código complexo em Elixir que realiza a geração de uma árvore binária balanceada a partir de uma lista de elementos:

```elixir
defmodule Arvore do
  defstruct valor: nil, esquerda: nil, direita: nil

  def criar_arvore(lista) do
    meio = div(length(lista), 2)
    valor = List.at(lista, meio)

    esquerda = criar_arvore(List.slice(lista, 0, meio))
    direita = criar_arvore(List.slice(lista, meio + 1, length(lista)))

    %Arvore{valor: valor, esquerda: esquerda, direita: direita}
  end

  def imprimir_arvore(arvore) do
    imprimir_arvore_recursivo(arvore, 0)
  end

  defp imprimir_arvore_recursivo(%Arvore{valor: valor, esquerda: esquerda, direita: direita}, nivel) do
    IO.puts String.duplicate("  ", nivel) <> "#{valor}"
    if esquerda do
      imprimir_arvore_recursivo(esquerda, nivel + 1)
    end
    if direita do
      imprimir_arvore_recursivo(direita, nivel + 1)
    end
  end
end

lista = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
arvore = Arvore.criar_arvore(lista)
Arvore.imprimir_arvore(arvore)
```

Neste código, temos a definição de um módulo chamado `Arvore` que contém as funções `criar_arvore` e `imprimir_arvore`. O objetivo é criar uma árvore binária balanceada a partir de uma lista de elementos e em seguida imprimir a árvore na tela.

A função `criar_arvore` recebe uma lista como parâmetro e utiliza recursão para dividir a lista ao meio e criar as subárvores esquerda e direita. O elemento do meio da lista é escolhido como o valor do nó atual. Essa divisão ocorre até que a lista seja totalmente percorrida.

A função `imprimir_arvore` chama a função `imprimir_arvore_recursivo` para imprimir a árvore de forma hierárquica. A função `imprimir_arvore_recursivo` recebe um nó da árvore, o nível atual e imprime o valor do nó. Em seguida, verifica se existem subárvores esquerda e direita e chama recursivamente a função para imprimir essas subárvores, aumentando o nível em 1 a cada chamada.

No final, é criada uma lista de números de 1 a 10 e a função `criar_arvore` é chamada passando essa lista como parâmetro. O resultado é atribuído à variável `arvore` e, em seguida, a função `imprimir_arvore` é chamada para exibir a árvore na tela.