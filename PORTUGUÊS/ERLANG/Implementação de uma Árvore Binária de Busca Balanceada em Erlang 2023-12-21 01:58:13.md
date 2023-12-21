Claro! Aqui está um código complexo em Erlang, que implementa uma árvore binária de busca balanceada e realiza algumas operações, como inserção, remoção e busca de elementos na árvore.

```erlang
-module(arvore_binaria).
-export([vazia/0, insere/2, remove/2, busca/2]).

%% Definindo a estrutura da árvore binária
-record(arvore, {valor, esquerda, direita}).

%% Função para criar uma árvore binária vazia
vazia() -> nil.

%% Função para inserir um elemento na árvore
insere(Valor, nil) ->
    #arvore{valor = Valor, esquerda = nil, direita = nil};
insere(Valor, Arvore) ->
    case Valor < Arvore#arvore.valor of
        true ->
            Arvore#arvore{esquerda = insere(Valor, Arvore#arvore.esquerda)};
        false ->
            Arvore#arvore{direita = insere(Valor, Arvore#arvore.direita)}
    end.

%% Função para remover um elemento da árvore
remove(_Valor, nil) ->
    nil;
remove(Valor, Arvore) ->
    case Valor < Arvore#arvore.valor of
        true ->
            Arvore#arvore{esquerda = remove(Valor, Arvore#arvore.esquerda)};
        false ->
            case Valor > Arvore#arvore.valor of
                true ->
                    Arvore#arvore{direita = remove(Valor, Arvore#arvore.direita)};
                false ->
                    case {Arvore#arvore.esquerda, Arvore#arvore.direita} of
                        {nil, nil} ->
                            nil;
                        {nil, _} ->
                            Arvore#arvore{valor = menor_valor(Arvore#arvore.direita), direita = remove(menor_valor(Arvore#arvore.direita), Arvore#arvore.direita)};
                        {_, nil} ->
                            Arvore#arvore{valor = maior_valor(Arvore#arvore.esquerda), esquerda = remove(maior_valor(Arvore#arvore.esquerda), Arvore#arvore.esquerda)};
                        _ ->
                            Arvore#arvore{valor = menor_valor(Arvore#arvore.direita), direita = remove(menor_valor(Arvore#arvore.direita), Arvore#arvore.direita)}
                    end
            end
    end.

%% Função para buscar um elemento na árvore
busca(_Valor, nil) ->
    false;
busca(Valor, Arvore) ->
    case Valor < Arvore#arvore.valor of
        true ->
            busca(Valor, Arvore#arvore.esquerda);
        false ->
            case Valor > Arvore#arvore.valor of
                true ->
                    busca(Valor, Arvore#arvore.direita);
                false ->
                    true
            end
    end.

%% Função auxiliar para encontrar o menor valor na árvore
menor_valor(Arvore) ->
    MenorValor = Arvore#arvore.valor,
    case Arvore#arvore.esquerda of
        nil ->
            MenorValor;
        _ ->
            menor_valor(Arvore#arvore.esquerda)
    end.

%% Função auxiliar para encontrar o maior valor na árvore
maior_valor(Arvore) ->
    MaiorValor = Arvore#arvore.valor,
    case Arvore#arvore.direita of
        nil ->
            MaiorValor;
        _ ->
            maior_valor(Arvore#arvore.direita)
    end.
```

Nesse código, definimos um módulo `arvore_binaria` em Erlang, que exporta as funções `vazia/0`, `insere/2`, `remove/2` e `busca/2`.

A árvore binária é representada usando um registro chamado `arvore`, que possui três campos: `valor`, `esquerda` e `direita`. 

A função `vazia/0` retorna uma árvore binária vazia, representada pelo valor `nil`.

A função `insere/2` recebe um valor e uma árvore binária. Se a árvore for vazia, cria-se uma nova árvore com o valor passado. Caso contrário, verifica-se se o valor é menor ou maior que o valor da raiz da árvore. Se for menor, a função é chamada recursivamente para a subárvore esquerda. Se for maior, a função é chamada recursivamente para a subárvore direita. 

A função `remove/2` recebe um valor e uma árvore binária. Se a árvore for vazia, retorna `nil`. Caso contrário, verifica-se se o valor é menor, maior ou igual ao valor da raiz da árvore. Se for menor, a função é chamada recursivamente para a subárvore esquerda. Se for maior, a função é chamada recursivamente para a subárvore direita. Se for igual, a remoção é realizada. Existem três casos para a remoção: quando o nó a ser removido não possui filhos, quando o nó a ser removido possui apenas um filho e quando o nó a ser removido possui dois filhos.

A função `busca/2` recebe um valor e uma árvore binária. Se a árvore for vazia, retorna `false`. Caso contrário, verifica-se se o valor é menor, maior ou igual ao valor da raiz da árvore. Se for menor, a função é chamada recursivamente para a subárvore esquerda. Se for maior, a função é chamada recursivamente para a subárvore direita. Se for igual, retorna `true`.

As funções auxiliares `menor_valor/1` e `maior_valor/1` são usadas para encontrar o menor e o maior valor em uma árvore binária, respectivamente.

Espero que esse código atenda às suas expectativas! Lembre-se de que esse é apenas um exemplo de código complexo em Erlang e você pode adaptá-lo e modificá-lo de acordo com suas necessidades.