Claro! Aqui está um exemplo de código complexo em Elixir, que implementa um sistema de cadastro de usuários usando uma estrutura de dados de árvores binárias:

```elixir
defmodule ArvoreCadastro do
  defstruct [:chave, :valor, :esquerda, :direita]

  def inserir(arvore, chave, valor) do
    case arvore do
      nil ->
        %ArvoreCadastro{chave: chave, valor: valor, esquerda: nil, direita: nil}
      %ArvoreCadastro{chave: chave} = no ->
        %ArvoreCadastro{no | valor: valor}
      %ArvoreCadastro{chave: no_chave} = no ->
        if chave < no_chave do
          %ArvoreCadastro{no | esquerda: inserir(no.esquerda, chave, valor)}
        else
          %ArvoreCadastro{no | direita: inserir(no.direita, chave, valor)}
        end
    end
  end

  def buscar(arvore, chave) do
    case arvore do
      nil ->
        nil
      %ArvoreCadastro{chave: chave, valor: valor} ->
        valor
      %ArvoreCadastro{chave: no_chave} ->
        if chave < no_chave do
          buscar(no.esquerda, chave)
        else
          buscar(no.direita, chave)
        end
    end
  end

  def imprimir(arvore) do
    case arvore do
      nil ->
        IO.puts("Árvore vazia.")
      %ArvoreCadastro{chave: chave, valor: valor, esquerda: esquerda, direita: direita} ->
        imprimir(esquerda)
        IO.puts("#{chave}: #{valor}")
        imprimir(direita)
    end
  end
end

# Exemplo de uso
arvore = nil
arvore = ArvoreCadastro.inserir(arvore, "usuario1", %{nome: "João", idade: 25})
arvore = ArvoreCadastro.inserir(arvore, "usuario2", %{nome: "Maria", idade: 30})
arvore = ArvoreCadastro.inserir(arvore, "usuario3", %{nome: "Pedro", idade: 20})

ArvoreCadastro.imprimir(arvore)

valor = ArvoreCadastro.buscar(arvore, "usuario2")
IO.inspect(valor)
```

Neste código, é criado um módulo chamado `ArvoreCadastro` que implementa as funcionalidades de inserção, busca e impressão de uma árvore binária de cadastro de usuários.

A função `inserir` recebe uma árvore em formato de registro `ArvoreCadastro`, uma chave única para o usuário e um valor (que pode ser um mapa contendo informações do usuário, por exemplo). Caso a árvore esteja vazia (`nil`), é criado um novo nó com a chave e valor fornecidos. Caso contrário, é verificado se a chave já existe na árvore. Se sim, o valor do nó existente é atualizado. Caso contrário, é realizada uma comparação entre as chaves para decidir se o novo nó será inserido à esquerda ou à direita do nó atual.

A função `buscar` recebe uma árvore e uma chave, e retorna o valor correspondente à chave fornecida. Ela percorre a árvore de forma recursiva, comparando as chaves dos nós para encontrar o nó desejado. Caso o nó seja encontrado, o valor correspondente é retornado. Caso contrário, é retornado `nil`.

A função `imprimir` recebe uma árvore e imprime os pares chave-valor em ordem crescente. Ela utiliza uma abordagem de percurso em ordem, onde primeiro são impressos os nós da subárvore esquerda, seguidos pelo nó atual e, por fim, os nós da subárvore direita.

No exemplo de uso, é criada uma árvore vazia e são inseridos três usuários com suas respectivas chaves e valores. Em seguida, a árvore é impressa e é realizada uma busca pelo valor correspondente à chave "usuario2". O resultado é exibido no terminal.

Este código é apenas um exemplo de implementação e pode ser adaptado conforme as necessidades específicas do projeto.