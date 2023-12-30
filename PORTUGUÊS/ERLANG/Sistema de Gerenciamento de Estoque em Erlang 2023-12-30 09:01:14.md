Claro! Aqui está um exemplo de código em Erlang. Este código é um sistema completo de gerenciamento de estoque para uma loja fictícia.

```erlang
-module(gerenciamento_estoque).
-export([start/0, adicionar_produto/3, remover_produto/2, atualizar_quantidade/2, listar_produtos/0]).

% Inicializa o sistema de estoque com uma lista vazia
start() ->
    io:format("Sistema de Gerenciamento de Estoque iniciado.~n"),
    {[], 0}.

% Adiciona um produto ao estoque
adicionar_produto({Codigo, Descricao, Preco}, Quantidade, Estoque) ->
    NovoEstoque = adicionar_ou_atualizar_produto({Codigo, Descricao, Preco}, Quantidade, Estoque),
    io:format("Produto ~w adicionado ao estoque.~n", [Descricao]),
    NovoEstoque.

% Remove um produto do estoque
remover_produto(Codigo, Estoque) ->
    NovoEstoque = remover_produto_aux(Codigo, Estoque),
    io:format("Produto removido do estoque.~n"),
    NovoEstoque.

% Atualiza a quantidade de um produto no estoque
atualizar_quantidade({Codigo, Quantidade}, Estoque) ->
    case obter_produto(Codigo, Estoque) of
        {Produto, _} ->
            NovoEstoque = adicionar_ou_atualizar_produto(Produto, Quantidade, Estoque),
            io:format("Quantidade do produto ~w atualizada para ~w.~n", [Produto#produto.descricao, Quantidade]),
            NovoEstoque;
        _ ->
            io:format("Produto não encontrado no estoque.~n"),
            Estoque
    end.

% Lista todos os produtos no estoque
listar_produtos() ->
    Estoque = start(),
    listar_produtos_aux(Estoque).

% Função auxiliar para adicionar ou atualizar um produto no estoque
adicionar_ou_atualizar_produto(Produto, Quantidade, {Produtos, Total}) ->
    case lists:keyfind(Produto#produto.codigo, 1, Produtos) of
        false ->
            NovoProduto = Produto#produto{quantidade = Quantidade},
            NovosProdutos = [NovoProduto | Produtos],
            NovoTotal = Total + Quantidade,
            {NovosProdutos, NovoTotal};
        {Codigo, Descricao, Preco, _} ->
            NovoProduto = Produto#produto{quantidade = Produto#produto.quantidade + Quantidade},
            NovosProdutos = lists:keyreplace(Codigo, 1, Produtos, {Codigo, Descricao, Preco, NovoProduto#produto.quantidade}),
            NovoTotal = Total + Quantidade,
            {NovosProdutos, NovoTotal}
    end.

% Função auxiliar para remover um produto do estoque
remover_produto_aux(Codigo, {Produtos, Total}) ->
    case lists:keyfind(Codigo, 1, Produtos) of
        false ->
            io:format("Produto não encontrado no estoque.~n"),
            {Produtos, Total};
        {_, Descricao, _, Quantidade} ->
            NovosProdutos = lists:keydelete(Codigo, 1, Produtos),
            NovoTotal = Total - Quantidade,
            {NovosProdutos, NovoTotal}
    end.

% Função auxiliar para obter um produto do estoque
obter_produto(Codigo, {Produtos, _}) ->
    lists:keyfind(Codigo, 1, Produtos).

% Função auxiliar para listar todos os produtos no estoque
listar_produtos_aux({Produtos, Total}) ->
    io:format("Produtos no estoque:~n"),
    listar_produtos_aux(Produtos),
    io:format("Total de produtos: ~w~n", [Total]).

% Função auxiliar para listar os produtos no estoque
listar_produtos_aux([]) ->
    io:format("Nenhum produto no estoque.~n");
listar_produtos_aux([{Codigo, Descricao, Preco, Quantidade} | Produtos]) ->
    io:format("~w - ~w (R$~.2f) - Quantidade: ~w~n", [Codigo, Descricao, Preco, Quantidade]),
    listar_produtos_aux(Produtos).
```

Este código define um módulo chamado `gerenciamento_estoque` que exporta várias funções para gerenciar um estoque de produtos.

- A função `start/0` inicializa o sistema de estoque, retornando uma estrutura vazia de estoque.
- A função `adicionar_produto/3` permite adicionar um produto ao estoque, fornecendo o código, descrição, preço e quantidade do produto.
- A função `remover_produto/2` permite remover um produto do estoque, fornecendo o código do produto.
- A função `atualizar_quantidade/2` permite atualizar a quantidade de um produto no estoque, fornecendo o código do produto e a nova quantidade.
- A função `listar_produtos/0` lista todos os produtos no estoque.

O código utiliza o paradigma funcional do Erlang e faz uso de listas e casamento de padrões para realizar as operações de manipulação do estoque.

Você pode chamar as funções deste módulo diretamente no shell do Erlang para testar o sistema de gerenciamento de estoque. Por exemplo, você pode adicionar produtos ao estoque usando a função `adicionar_produto/3`, remover produtos usando a função `remover_produto/2`, atualizar a quantidade usando a função `atualizar_quantidade/2` e listar todos os produtos no estoque usando a função `listar_produtos/0`.