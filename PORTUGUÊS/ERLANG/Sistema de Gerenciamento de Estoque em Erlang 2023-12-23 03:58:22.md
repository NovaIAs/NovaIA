Claro! Aqui está um código complexo em Erlang que implementa um sistema de gerenciamento de estoque para uma loja:

```erlang
-module(gerenciamento_estoque).
-export([iniciar/0, adicionar_item/3, remover_item/2, listar_estoque/0]).

%% Estado inicial do estoque (vazio)
iniciar() ->
    {[], 0}.

%% Adicionar um item ao estoque
adicionar_item(Estoque, Item, Quantidade) ->
    {Itens, Total} = Estoque,
    NovoEstoque = adicionar_item_estoque(Item, Quantidade, Itens, Total),
    io:format("Item adicionado com sucesso.~n"),
    NovoEstoque.

%% Remover um item do estoque
remover_item(Estoque, Item) ->
    {Itens, Total} = Estoque,
    NovoEstoque = remover_item_estoque(Item, Itens, Total),
    io:format("Item removido com sucesso.~n"),
    NovoEstoque.

%% Listar o estoque atual
listar_estoque() ->
    Estoque = iniciar(),
    {Itens, Total} = Estoque,
    listar_itens_estoque(Itens),
    io:format("Total de itens em estoque: ~p~n", [Total]).

%% Adicionar um item ao estoque
adicionar_item_estoque(Item, Quantidade, [], Total) ->
    [{Item, Quantidade}],
    Quantidade + Total;
adicionar_item_estoque(Item, Quantidade, [{Item, Estoque} | Resto], Total) ->
    NovaQuantidade = Quantidade + Estoque,
    [{Item, NovaQuantidade} | Resto],
    Quantidade + Total;
adicionar_item_estoque(Item, Quantidade, [ItemAtual | Resto], Total) ->
    adicionar_item_estoque(Item, Quantidade, Resto, Total).

%% Remover um item do estoque
remover_item_estoque(Item, [], Total) ->
    io:format("Item não encontrado no estoque.~n"),
    [],
    Total;
remover_item_estoque(Item, [{Item, Estoque} | Resto], Total) ->
    [{Item, 0} | Resto],
    Estoque + Total;
remover_item_estoque(Item, [ItemAtual | Resto], Total) ->
    remover_item_estoque(Item, Resto, Total).

%% Listar os itens do estoque
listar_itens_estoque([]) ->
    io:format("Estoque vazio.~n");
listar_itens_estoque([{Item, Estoque} | Resto]) ->
    io:format("~p - Quantidade: ~p~n", [Item, Estoque]),
    listar_itens_estoque(Resto).
```

Explicação do código:
1. O código começa definindo o módulo `gerenciamento_estoque`.
2. A função `iniciar/0` é responsável por inicializar o estoque vazio.
3. A função `adicionar_item/3` permite adicionar um item ao estoque, recebendo como parâmetros o estoque atual, o item a ser adicionado e a quantidade desse item.
4. A função `remover_item/2` permite remover um item do estoque, recebendo como parâmetros o estoque atual e o item a ser removido.
5. A função `listar_estoque/0` lista todos os itens presentes no estoque atual, bem como a quantidade total de itens.
6. As funções auxiliares `adicionar_item_estoque/4` e `remover_item_estoque/3` são responsáveis por atualizar o estoque, adicionando ou removendo um item.
7. A função `listar_itens_estoque/1` é responsável por percorrer e listar todos os itens presentes no estoque.
8. O código utiliza operações de listas e recursão para manipular o estoque.
9. Mensagens informativas são exibidas para o usuário através da função `io:format/2`.

Espero que este código atenda às suas expectativas de complexidade e diferenciação.