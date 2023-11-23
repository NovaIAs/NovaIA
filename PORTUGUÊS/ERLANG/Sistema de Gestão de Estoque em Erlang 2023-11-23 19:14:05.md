Claro! Aqui está um código em Erlang complexo e diferenciado. Este código simula um sistema de gestão de estoque em uma loja virtual, implementando recursos como adicionar produtos ao estoque, remover produtos do estoque, consultar a quantidade de um determinado produto, entre outros. 

```erlang
-module(loja_virtual).
-export([iniciar/0, adicionar_produto/2, remover_produto/2, consultar_quantidade/1]).

% Variável global para armazenar o estoque
-record(produto, {nome, quantidade}).
-define(ESTOQUE, gb_sets:new()).

% Função de inicialização do estoque
iniciar() ->
    {ok, ets:new(?ESTOQUE, [set, named_table, public])}.

% Função para adicionar um produto ao estoque
adicionar_produto(Nome, Quantidade) ->
    #produto{nome = Nome, quantidade = Quantidade} = NovoProduto,
    ets:insert(?ESTOQUE, NovoProduto),
    ok.

% Função para remover um produto do estoque
remover_produto(Nome, Quantidade) ->
    case ets:lookup(?ESTOQUE, Nome) of
        [{_, EstoqueAtual}] when EstoqueAtual >= Quantidade ->
            NovoEstoque = EstoqueAtual - Quantidade,
            case NovoEstoque of
                0 -> 
                    ets:delete(?ESTOQUE, Nome),
                    ok;
                _ ->
                    ets:update_element(?ESTOQUE, {Nome, '_'}, {Nome, NovoEstoque}),
                    ok
            end;
        _ ->
            error
    end.

% Função para consultar a quantidade de um produto no estoque
consultar_quantidade(Nome) ->
    case ets:lookup(?ESTOQUE, Nome) of
        [{_, Quantidade}] -> Quantidade;
        _ -> 0
    end.
```

Neste código, utilizamos registros (records) para definir a estrutura de um produto, contendo os campos "nome" e "quantidade". O estoque é implementado utilizando uma tabela ets (Erlang Term Storage), que é uma estrutura de dados otimizada para armazenar grandes quantidades de informações.

A função `iniciar/0` é responsável por inicializar o estoque, criando a tabela ets e atribuindo-a à variável global `?ESTOQUE`.

A função `adicionar_produto/2` recebe o nome e a quantidade de um produto como argumentos, cria um novo registro do tipo `produto` e insere-o na tabela ets.

A função `remover_produto/2` recebe o nome e a quantidade de um produto a ser removido do estoque. Ela verifica se o produto existe no estoque e se a quantidade a ser removida é menor ou igual à quantidade disponível. Caso essas condições sejam atendidas, a função atualiza a quantidade do produto no estoque.

A função `consultar_quantidade/1` recebe o nome de um produto como argumento e retorna a quantidade disponível no estoque para esse produto.

Espero que você encontre utilidade neste código complexo e diferenciado em Erlang!