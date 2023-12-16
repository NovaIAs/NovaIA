Claro! Aqui está um exemplo de código complexo em Erlang. Este código é um simulador de voo espacial, onde vamos criar um módulo chamado 'espaco' que irá controlar a nave espacial e suas diversas funcionalidades.

```erlang
-module(espaco).
-export([iniciar/0, decolar/0, pousar/0, mudar_rotas/1, verificar_combustivel/0]).

% Variáveis globais
-define(VELOCIDADE_MAXIMA, 30000).
-define(DISTANCIA_MAXIMA, 1000000).

% Estado inicial
-record(nave, {posicao, velocidade, combustivel}).

% Função iniciar/0
iniciar() ->
    Nave = #nave{posicao = {0, 0, 0}, velocidade = 0, combustivel = 100},
    io:format("Nave espacial iniciada.~n"),
    io:format("Posição atual: ~p~n", [Nave#nave.posicao]),
    io:format("Velocidade atual: ~p~n", [Nave#nave.velocidade]),
    io:format("Combustível atual: ~p~n", [Nave#nave.combustivel]).

% Função decolar/0
decolar() ->
    Nave = iniciar(),
    NaveAtualizada = Nave#nave{velocidade = ?VELOCIDADE_MAXIMA},
    io:format("Decolagem realizada.~n"),
    io:format("Nova velocidade: ~p~n", [NaveAtualizada#nave.velocidade]).

% Função pousar/0
pousar() ->
    Nave = iniciar(),
    NaveAtualizada = Nave#nave{velocidade = 0},
    io:format("Pouso realizado.~n"),
    io:format("Nova velocidade: ~p~n", [NaveAtualizada#nave.velocidade]).

% Função mudar_rotas/1
mudar_rotas(Distancia) ->
    Nave = iniciar(),
    if
        Distancia < ?DISTANCIA_MAXIMA ->
            io:format("Nova rota definida para ~p unidades de distância.~n", [Distancia]),
            io:format("Posição atualizada: ~p~n", [Nave#nave.posicao]),
            io:format("Nova velocidade: ~p~n", [Nave#nave.velocidade]),
            io:format("Combustível atual: ~p~n", [Nave#nave.combustivel]);
        true ->
            io:format("Distância inválida. Não é possível mudar para essa rota.~n")
    end.

% Função verificar_combustivel/0
verificar_combustivel() ->
    Nave = iniciar(),
    if
        Nave#nave.combustivel > 50 ->
            io:format("Combustível suficiente.~n"),
            io:format("Combustível atual: ~p~n", [Nave#nave.combustivel]);
        Nave#nave.combustivel > 25 ->
            io:format("Combustível em nível baixo.~n"),
            io:format("Combustível atual: ~p~n", [Nave#nave.combustivel]);
        true ->
            io:format("Combustível esgotado.~n"),
            io:format("Combustível atual: ~p~n", [Nave#nave.combustivel])
    end.
```

Explicação do código:

1. Começamos definindo o módulo `espaco` e suas funções exportadas (`iniciar/0`, `decolar/0`, `pousar/0`, `mudar_rotas/1`, `verificar_combustivel/0`).
2. Em seguida, definimos as constantes `VELOCIDADE_MAXIMA` e `DISTANCIA_MAXIMA` usando a diretiva `-define`.
3. Em seguida, definimos o estado inicial da nave espacial usando o registro `nave` com os campos `posicao`, `velocidade` e `combustivel`.
4. A função `iniciar/0` cria uma nova nave espacial, imprime informações iniciais como posição, velocidade e combustível, e retorna o estado atualizado da nave.
5. A função `decolar/0` chama a função `iniciar/0`, atualiza a velocidade da nave para a velocidade máxima definida, e imprime a nova velocidade.
6. A função `pousar/0` chama a função `iniciar/0`, redefine a velocidade da nave para zero e imprime a nova velocidade.
7. A função `mudar_rotas/1` recebe uma distância como argumento, chama a função `iniciar/0` e, em seguida, verifica se a distância é menor que a distância máxima permitida. Se for, imprime a nova rota, a posição atualizada da nave, a nova velocidade e o combustível atual. Caso contrário, imprime uma mensagem informando que a distância é inválida.
8. A função `verificar_combustivel/0` chama a função `iniciar/0` e verifica o nível de combustível da nave. Dependendo do nível, imprime uma mensagem adequada.

Esse código cria uma estrutura de nave espacial com várias funcionalidades, permitindo a inicialização da nave, decolagem, pouso, mudança de rotas e verificação do nível de combustível.