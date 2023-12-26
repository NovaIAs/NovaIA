Claro! Aqui está um exemplo de código complexo em Erlang, que implementa um sistema de votação online para uma eleição:

```erlang
-module(sistema_votacao).

-export([iniciar/0, adicionar_eleitor/2, iniciar_votacao/1, votar/2, encerrar_votacao/0]).

-record(eleitor, {nome, idade, ja_votou}).
-record(candidato, {nome, votos}).

iniciar() ->
    io:fwrite("Sistema de Votação Online iniciado.~n"),
    {[], []}.

adicionar_eleitor(Eleitores, {Nome, Idade}) ->
    Eleitor = #eleitor{nome = Nome, idade = Idade, ja_votou = false},
    io:fwrite("Eleitor adicionado: ~s~n", [Nome]),
    {Eleitores ++ [Eleitor], []}.

iniciar_votacao(Candidatos) ->
    io:fwrite("Votação iniciada.~n"),
    listar_candidatos(Candidatos),
    {Candidatos, []}.

votar({Candidatos, Eleitores}, {NomeEleitor, NomeCandidato}) ->
    Eleitor = buscar_eleitor(Eleitores, NomeEleitor),
    Candidato = buscar_candidato(Candidatos, NomeCandidato),
    case Eleitor of
        false ->
            io:fwrite("Eleitor não encontrado.~n");
        _ ->
            case Candidato of
                false ->
                    io:fwrite("Candidato não encontrado.~n");
                _ ->
                    case Eleitor#eleitor.ja_votou of
                        true ->
                            io:fwrite("Este eleitor já votou.~n");
                        false ->
                            io:fwrite("Voto registrado.~n"),
                            AtualizarEleitor = Eleitor#eleitor{ja_votou = true},
                            AtualizarCandidato = Candidato#candidato{votos = Candidato#candidato.votos + 1},
                            NovosEleitores = atualizar_eleitores(Eleitores, NomeEleitor, AtualizarEleitor),
                            NovosCandidatos = atualizar_candidatos(Candidatos, NomeCandidato, AtualizarCandidato),
                            {NovosCandidatos, NovosEleitores}
                    end
            end
    end.

encerrar_votacao({Candidatos, Eleitores}) ->
    io:fwrite("Votação encerrada. Resultado final:~n"),
    listar_candidatos(Candidatos),
    io:fwrite("Eleitores que não votaram:~n"),
    listar_eleitores(Eleitores).

listar_candidatos(Candidatos) ->
    [io:fwrite("- ~s: ~B votos~n", [Candidato#candidato.nome, Candidato#candidato.votos]) || Candidato <- Candidatos].

listar_eleitores(Eleitores) ->
    [io:fwrite("- ~s~n", [Eleitor#eleitor.nome]) || Eleitor <- Eleitores].

buscar_eleitor(Eleitores, Nome) ->
    lists:nthtail(1, [Eleitor || Eleitor <- Eleitores, Eleitor#eleitor.nome == Nome]).

buscar_candidato(Candidatos, Nome) ->
    lists:nthtail(1, [Candidato || Candidato <- Candidatos, Candidato#candidato.nome == Nome]).

atualizar_eleitores(Eleitores, Nome, Eleitor) ->
    [Eleitor || Eleitor <- Eleitores, Eleitor#eleitor.nome /= Nome] ++ [Eleitor].

atualizar_candidatos(Candidatos, Nome, Candidato) ->
    [Candidato || Candidato <- Candidatos, Candidato#candidato.nome /= Nome] ++ [Candidato].
```

Este código implementa um sistema de votação online em Erlang. Ele permite adicionar eleitores, iniciar a votação com uma lista de candidatos, registrar votos e encerrar a votação exibindo o resultado final.

O módulo `sistema_votacao` possui várias funções exportadas que podem ser chamadas externamente. Ao chamar a função `iniciar()`, o sistema é inicializado e retorna uma tupla vazia com as listas de eleitores e candidatos.

A função `adicionar_eleitor()` é usada para adicionar um eleitor à lista de eleitores, recebendo como parâmetros o nome e a idade do eleitor. A função retorna uma nova tupla atualizada com o eleitor adicionado.

A função `iniciar_votacao()` inicia a votação e recebe uma lista de candidatos como parâmetro. Ela exibe a lista de candidatos na console.

A função `votar()` é usada para registrar um voto. Ela recebe como parâmetros a tupla contendo as listas de candidatos e eleitores, e o nome do eleitor e do candidato escolhido. A função verifica se o eleitor e o candidato existem nas respectivas listas e se o eleitor já votou. Se todas essas condições forem atendidas, o voto é registrado, atualizando as listas de eleitores e candidatos.

A função `encerrar_votacao()` encerra a votação e exibe o resultado final, mostrando os candidatos e seus respectivos votos, além de listar os eleitores que não votaram.

As funções auxiliares `listar_candidatos()` e `listar_eleitores()` são usadas para exibir os candidatos e eleitores na console.

As funções auxiliares `buscar_eleitor()` e `buscar_candidato()` são usadas para buscar um eleitor ou candidato específico nas listas.

As funções auxiliares `atualizar_eleitores()` e `atualizar_candidatos()` são usadas para atualizar as listas de eleitores e candidatos, respectivamente.

Espero que este código atenda às suas expectativas de complexidade e diferenciação.