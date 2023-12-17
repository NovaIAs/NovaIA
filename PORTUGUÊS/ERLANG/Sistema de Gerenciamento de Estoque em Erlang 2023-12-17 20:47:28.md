Claro! Aqui está um código em Erlang que implementa um sistema de gerenciamento de estoque de uma loja fictícia. O código utiliza processos em Erlang para simular o funcionamento do estoque, permitindo o registro de produtos, compra, venda e atualização dos estoques.

```erlang
-module(estoque).
-export([start/0, stop/0, registrar_produto/2, comprar_produto/2, vender_produto/2, atualizar_estoque/1]).

start() ->
    spawn(fun() -> loop([]) end).

stop() ->
    ?MODULE ! stop.

registrar_produto(Produto, Quantidade) ->
    ?MODULE ! {registrar_produto, Produto, Quantidade}.

comprar_produto(Produto, Quantidade) ->
    ?MODULE ! {comprar_produto, Produto, Quantidade}.

vender_produto(Produto, Quantidade) ->
    ?MODULE ! {vender_produto, Produto, Quantidade}.

atualizar_estoque(Estoque) ->
    ?MODULE ! {atualizar_estoque, Estoque}.

loop(Estoque) ->
    receive
        {registrar_produto, Produto, Quantidade} ->
            NovoEstoque = registrar_produto(Produto, Quantidade, Estoque),
            loop(NovoEstoque);
        {comprar_produto, Produto, Quantidade} ->
            NovoEstoque = comprar_produto(Produto, Quantidade, Estoque),
            loop(NovoEstoque);
        {vender_produto, Produto, Quantidade} ->
            NovoEstoque = vender_produto(Produto, Quantidade, Estoque),
            loop(NovoEstoque);
        {atualizar_estoque, NovoEstoque} ->
            loop(NovoEstoque);
        stop ->
            ok
    end.

registrar_produto(Produto, Quantidade, Estoque) ->
    case lists:keyfind(Produto, 1, Estoque) of
        false ->
            [{Produto, Quantidade} | Estoque];
        {Produto, EstoqueAtual} ->
            EstoqueAtualizado = EstoqueAtual + Quantidade,
            lists:keyreplace(Produto, 1, Estoque, {Produto, EstoqueAtualizado})
    end.

comprar_produto(Produto, Quantidade, Estoque) ->
    case lists:keyfind(Produto, 1, Estoque) of
        false ->
            Estoque;
        {Produto, EstoqueAtual} ->
            EstoqueAtualizado = EstoqueAtual - Quantidade,
            lists:keyreplace(Produto, 1, Estoque, {Produto, EstoqueAtualizado})
    end.

vender_produto(Produto, Quantidade, Estoque) ->
    case lists:keyfind(Produto, 1, Estoque) of
        false ->
            Estoque;
        {Produto, EstoqueAtual} ->
            EstoqueAtualizado = EstoqueAtual + Quantidade,
            lists:keyreplace(Produto, 1, Estoque, {Produto, EstoqueAtualizado})
    end.
```

Este código implementa um módulo chamado `estoque` em Erlang. Ele possui as seguintes funções:

- `start/0`: inicia o estoque do sistema.
- `stop/0`: encerra o estoque do sistema.
- `registrar_produto/2`: recebe o nome de um produto e a quantidade a ser registrada no estoque.
- `comprar_produto/2`: recebe o nome de um produto e a quantidade a ser comprada do estoque.
- `vender_produto/2`: recebe o nome de um produto e a quantidade a ser vendida do estoque.
- `atualizar_estoque/1`: recebe uma lista de produtos e quantidades para atualizar o estoque.

O estoque é implementado como uma lista de pares `{Produto, Quantidade}`. Cada operação de registro, compra ou venda é realizada através de um processo em Erlang, que atualiza o estoque de forma concorrente.

O loop principal do estoque recebe as mensagens enviadas pelas funções, realiza a operação correspondente e atualiza o estoque. Em seguida, o loop continua aguardando novas mensagens.

Espero que este exemplo tenha atendido às suas expectativas! Lembre-se de que a linguagem Erlang possui uma sintaxe específica e é amplamente utilizada para desenvolvimento de sistemas concorrentes e distribuídos.