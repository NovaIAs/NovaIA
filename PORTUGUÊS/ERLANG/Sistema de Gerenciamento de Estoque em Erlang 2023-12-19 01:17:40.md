Claro! Aqui está um exemplo de código em Erlang que implementa um sistema de gerenciamento de estoque de uma loja:

```erlang
-module(gerenciamento_estoque).
-behaviour(gen_server).

-export([start/0, adicionar_item/2, remover_item/2, listar_itens/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(item, {codigo, nome, quantidade}).

%%% API pública %%%

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

adicionar_item(Codigo, Nome, Quantidade) ->
    gen_server:call(?MODULE, {adicionar_item, Codigo, Nome, Quantidade}).

remover_item(Codigo, Quantidade) ->
    gen_server:call(?MODULE, {remover_item, Codigo, Quantidade}).

listar_itens() ->
    gen_server:call(?MODULE, listar_itens).

%%% Callbacks do gen_server %%%

init([]) ->
    {ok, []}.

handle_call({adicionar_item, Codigo, Nome, Quantidade}, _From, State) ->
    Item = #item{codigo = Codigo, nome = Nome, quantidade = Quantidade},
    NovoEstado = adicionar_item(Item, State),
    {reply, ok, NovoEstado};

handle_call({remover_item, Codigo, Quantidade}, _From, State) ->
    NovoEstado = remover_item(Codigo, Quantidade, State),
    {reply, ok, NovoEstado};

handle_call(listar_itens, _From, State) ->
    {reply, State, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%% Funções auxiliares %%%

adicionar_item(Item, State) ->
    [{codigo, Item#item.codigo, nome, Item#item.nome, quantidade, Item#item.quantidade} | State].

remover_item(Codigo, Quantidade, State) ->
    Atualizado = lists:keyreplace(codigo, 1, State, {codigo, Codigo, nome, '_', quantidade, '_'}),
    [{codigo, Codigo, nome, Nome, quantidade, NovaQuantidade} | Resto] = Atualizado,
    QuantidadeAtual = Quantidade + NovaQuantidade,
    if
        QuantidadeAtual =< 0 -> lists:keydelete(codigo, 1, Atualizado);
        true -> [{codigo, Codigo, nome, Nome, quantidade, QuantidadeAtual} | Resto]
    end.
```

Explicação do código:

- Na linha 5, declaramos o módulo `gerenciamento_estoque` e especificamos que ele é um `gen_server`, ou seja, um processo Erlang que implementa um servidor genérico.
- Nas linhas 7 e 8, exportamos as funções públicas `start/0`, `adicionar_item/2`, `remover_item/2` e `listar_itens/1`.
- A função `start/0` na linha 10 inicia o servidor.
- As funções `adicionar_item/3`, `remover_item/2` e `listar_itens/0` são as funções públicas que os usuários podem chamar para interagir com o sistema.
- O estado do servidor é representado por uma lista de itens, onde cada item é um registro com os campos `codigo`, `nome` e `quantidade`.
- A função `init/1` na linha 18 inicializa o estado vazio do servidor.
- As funções `handle_call/3`, `handle_cast/2`, `handle_info/2`, `terminate/2` e `code_change/3` são callbacks do `gen_server` que especificam como o servidor deve responder a diferentes tipos de mensagens e eventos.
- Na função `handle_call/3`, tratamos as mensagens de chamada (síncronas) recebidas pelo servidor. Na linha 22, quando recebemos uma mensagem `{adicionar_item, Codigo, Nome, Quantidade}`, criamos um novo item com os valores recebidos e adicionamos à lista de itens. Na linha 26, quando recebemos uma mensagem `{remover_item, Codigo, Quantidade}`, procuramos o item correspondente na lista e atualizamos sua quantidade. Na linha 30, quando recebemos uma mensagem `listar_itens`, nós respondemos ao chamador com o estado atual do servidor.
- A função `handle_cast/2` é usada para tratar mensagens de notificação (assíncronas), mas neste exemplo não precisamos implementar nada específico.
- A função `handle_info/2` é usada para tratar outros tipos de mensagens e neste exemplo também não precisamos implementar nada específico.
- A função `terminate/2` é chamada quando o servidor é encerrado e neste exemplo não precisamos fazer nada especial.
- A função `code_change/3` é chamada quando uma atualização de código é aplicada ao servidor, mas também não precisamos implementar nada específico aqui.
- As funções auxiliares `adicionar_item/2` e `remover_item/3` são responsáveis por adicionar e remover itens da lista de itens, respectivamente.
- No exemplo, a lista de itens é simplesmente uma lista de registros onde cada registro representa um item com os campos `codigo`, `nome` e `quantidade`. A função `adicionar_item/2` simplesmente adiciona um novo item à lista, enquanto a função `remover_item/3` procura o item correspondente na lista, atualiza sua quantidade e remove o item se a nova quantidade for menor ou igual a zero.
- Ao chamar as funções `adicionar_item/2`, `remover_item/2` e `listar_itens/0`, estamos enviando uma mensagem para o servidor usando a função `gen_server:call/2`, que é uma forma de comunicação síncrona.
- Por fim, quando um usuário chama uma das funções públicas, o servidor responde com um "ok" ou com o estado atual do servidor, dependendo do tipo de chamada.