Claro! Vou criar um código complexo em ERLANG para você. Este código será um simulador de um sistema de controle de tráfego aéreo. Vamos dividir o código em módulos para facilitar a compreensão.

1. Módulo "aeroporto":

```erlang
-module(aeroporto).
-export([iniciar/0]).

iniciar() ->
    io:format("Sistema de controle de tráfego aéreo iniciado.~n"),
    controle:aeronaves([]).
```

Neste módulo, definimos a função `iniciar/0`, que inicia o sistema de controle de tráfego aéreo chamando a função `controle:aeronaves/1` passando uma lista vazia como parâmetro.

2. Módulo "controle":

```erlang
-module(controle).
-export([aeronaves/1, adicionar_aeronave/2, remover_aeronave/2, listar_aeronaves/1]).

aeronaves(Aeronaves) ->
    io:format("Opções disponíveis:~n"),
    io:format("1. Adicionar aeronave~n"),
    io:format("2. Remover aeronave~n"),
    io:format("3. Listar aeronaves~n"),
    io:format("4. Encerrar sistema~n"),
    io:format("Escolha uma opção: "),
    Opcao = read_int(),
    case Opcao of
        1 ->
            {NovaAeronave, NovaLista} = adicionar_aeronave(Aeronaves),
            aeronaves(NovaLista);
        2 ->
            {RemoverAeronave, NovaLista} = remover_aeronave(Aeronaves),
            aeronaves(NovaLista);
        3 ->
            listar_aeronaves(Aeronaves),
            aeronaves(Aeronaves);
        4 ->
            io:format("Encerrando sistema de controle de tráfego aéreo.~n");
        _ ->
            io:format("Opção inválida. Por favor, escolha novamente.~n"),
            aeronaves(Aeronaves)
    end.

adicionar_aeronave(Aeronaves) ->
    io:format("Digite o código da aeronave: "),
    Codigo = read_atom(),
    io:format("Digite o nome do piloto: "),
    Piloto = read_line(),
    NovaAeronave = {Codigo, Piloto},
    NovaLista = Aeronaves ++ [NovaAeronave],
    io:format("Aeronave adicionada com sucesso.~n"),
    {NovaAeronave, NovaLista}.

remover_aeronave(Aeronaves) ->
    case Aeronaves of
        [] ->
            io:format("Não há aeronaves para remover.~n"),
            {undefined, []};
        _ ->
            io:format("Digite o código da aeronave a ser removida: "),
            Codigo = read_atom(),
            {RemoverAeronave, NovaLista} = remover_aeronave_helper(Aeronaves, Codigo),
            case RemoverAeronave of
                undefined ->
                    io:format("Aeronave não encontrada.~n");
                _ ->
                    io:format("Aeronave removida com sucesso.~n")
            end,
            {RemoverAeronave, NovaLista}
    end.

remover_aeronave_helper([], _) ->
    {undefined, []};
remover_aeronave_helper([{Codigo, Piloto} | Resto], Codigo) ->
    {Codigo, Resto};
remover_aeronave_helper([Aeronave | Resto], Codigo) ->
    {RemoverAeronave, NovaLista} = remover_aeronave_helper(Resto, Codigo),
    {[Aeronave | RemoverAeronave], NovaLista}.

listar_aeronaves([]) ->
    io:format("Não há aeronaves cadastradas.~n");
listar_aeronaves(Aeronaves) ->
    io:format("Aeronaves cadastradas:~n"),
    listar_aeronaves_helper(Aeronaves).

listar_aeronaves_helper([]) ->
    io:format("Fim da lista.~n");
listar_aeronaves_helper([{Codigo, Piloto} | Resto]) ->
    io:format("Código: ~w, Piloto: ~s~n", [Codigo, Piloto]),
    listar_aeronaves_helper(Resto).
```

Neste módulo, temos a função `aeronaves/1`, que exibe um menu de opções para o usuário e realiza as ações de acordo com a opção escolhida. As opções incluem adicionar aeronave, remover aeronave, listar aeronaves e encerrar o sistema.

As funções `adicionar_aeronave/2` e `remover_aeronave/2` são responsáveis por adicionar e remover aeronaves da lista de aeronaves, respectivamente. A função `listar_aeronaves/1` exibe a lista de aeronaves cadastradas.

3. Módulo "leitura":

```erlang
-module(leitura).
-export([read_line/0, read_int/0, read_atom/0]).

read_line() ->
    io:fread(""),
    io:get_line("").

read_int() ->
    {ok, [Int], _} = io:fread("", "~d"),
    Int.

read_atom() ->
    {ok, [Atom], _} = io:fread("", "~s"),
    list_to_atom(Atom).
```

Neste módulo, temos algumas funções auxiliares para ler entradas do usuário. As funções `read_line/0`, `read_int/0` e `read_atom/0` são responsáveis por ler uma linha de texto, um número inteiro e um átomo, respectivamente.

Espero que este código complexo em ERLANG atenda às suas expectativas!