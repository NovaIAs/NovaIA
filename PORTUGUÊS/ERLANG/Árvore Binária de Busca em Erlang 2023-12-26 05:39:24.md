Claro! Aqui está um exemplo de código complexo em Erlang, que cria uma estrutura de dados de uma árvore binária e implementa algumas operações básicas, como inserção, remoção e busca.

```erlang
-module(arvore_binaria).
-export([vazia/0, inserir/2, remover/2, buscar/2, percorrer/1]).

-record(arvore, {valor, esquerda, direita}).

%% Retorna uma árvore vazia
vazia() -> nil.

%% Insere um valor em uma árvore
inserir(Valor, nil) ->
    #arvore{valor = Valor, esquerda = nil, direita = nil};
inserir(Valor, #arvore{valor = V, esquerda = E, direita = D} = Arvore) when Valor < V ->
    Arvore#arvore{esquerda = inserir(Valor, E)};
inserir(Valor, #arvore{valor = V, esquerda = E, direita = D} = Arvore) when Valor > V ->
    Arvore#arvore{direita = inserir(Valor, D)};
inserir(_, Arvore) ->
    Arvore.

%% Remove um valor de uma árvore
remover(_, nil) ->
    nil;
remover(Valor, #arvore{valor = V, esquerda = E, direita = D} = Arvore) when Valor < V ->
    Arvore#arvore{esquerda = remover(Valor, E)};
remover(Valor, #arvore{valor = V, esquerda = E, direita = D} = Arvore) when Valor > V ->
    Arvore#arvore{direita = remover(Valor, D)};
remover(_, #arvore{valor = _, esquerda = nil, direita = nil}) ->
    nil;
remover(_, #arvore{valor = _, esquerda = nil, direita = D}) ->
    D;
remover(_, #arvore{valor = _, esquerda = E, direita = nil}) ->
    E;
remover(_, #arvore{valor = _, esquerda = E, direita = D}) ->
    Menor = busca_menor(D),
    #arvore{valor = Menor, esquerda = E, direita = remover(Menor, D)}.

%% Busca um valor em uma árvore
buscar(_, nil) ->
    false;
buscar(Valor, #arvore{valor = V, esquerda = _, direita = _}) when Valor == V ->
    true;
buscar(Valor, #arvore{valor = V, esquerda = E, direita = _}) when Valor < V ->
    buscar(Valor, E);
buscar(Valor, #arvore{valor = _, esquerda = _, direita = D}) ->
    buscar(Valor, D).

%% Percorre a árvore em ordem
percorrer(nil) ->
    [];
percorrer(#arvore{valor = V, esquerda = E, direita = D}) ->
    percorrer(E) ++ [V] ++ percorrer(D).

%% Função auxiliar para encontrar o menor valor da subárvore direita
busca_menor(#arvore{valor = V, esquerda = nil, direita = _}) ->
    V;
busca_menor(#arvore{valor = _, esquerda = E, direita = _}) ->
    busca_menor(E).
```

Neste código, definimos uma estrutura de dados `arvore` utilizando um registro Erlang, que possui três campos: `valor`, `esquerda` e `direita`. 

A função `vazia/0` retorna uma árvore vazia. A função `inserir/2` insere um valor em uma árvore, mantendo a ordem dos valores. A função `remover/2` remove um valor de uma árvore, levando em consideração os diferentes casos possíveis (remoção de uma folha, remoção de um nó com apenas um filho, remoção de um nó com dois filhos). A função `buscar/2` busca um valor em uma árvore e retorna `true` se encontrado ou `false` caso contrário. A função `percorrer/1` percorre a árvore em ordem e retorna uma lista com os valores.

O código também inclui uma função auxiliar `busca_menor/1` para encontrar o menor valor da subárvore direita durante a remoção de um nó com dois filhos.

Este código utiliza a recursão para percorrer a árvore e realizar as operações. Ele oferece uma estrutura básica para manipular árvores binárias de busca em Erlang.