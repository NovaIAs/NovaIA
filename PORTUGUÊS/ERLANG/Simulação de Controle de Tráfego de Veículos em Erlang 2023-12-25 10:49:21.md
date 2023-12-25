Claro! Aqui está um exemplo de um código complexo em Erlang que utiliza processos e mensagens para implementar uma simulação de um sistema de controle de tráfego de veículos em uma cidade:

```erlang
-module(controle_trafego).
-export([iniciar/0]).

-define(NUMERO_VEICULOS, 10).
-define(NUMERO_SEMAFOROS, 4).

%% Função iniciar/0
%% Inicia o sistema de controle de tráfego
iniciar() ->
    %% Inicializa semáforos
    Semaforos = iniciar_semaforos(),

    %% Inicializa veículos
    Veiculos = iniciar_veiculos(),

    %% Inicializa o controlador de tráfego
    spawn(fun() -> controlador_trafego(Semaforos, Veiculos) end).

%% Função iniciar_semaforos/0
%% Inicializa os semáforos do sistema
iniciar_semaforos() ->
    [spawn(fun() -> semaforo() end) || _ <- lists:seq(1, ?NUMERO_SEMAFOROS)].

%% Função iniciar_veiculos/0
%% Inicializa os veículos do sistema
iniciar_veiculos() ->
    [spawn(fun() -> veiculo() end) || _ <- lists:seq(1, ?NUMERO_VEICULOS)].

%% Função controlador_trafego/2
%% Controla o tráfego dos veículos na cidade
controlador_trafego(Semaforos, Veiculos) ->
    loop(Semaforos, Veiculos).

%% Função loop/2
%% Loop principal do controlador de tráfego
loop(Semaforos, Veiculos) ->
    receive
        {novas_coordenadas, Veiculo, Coordenadas} ->
            %% Atualiza as coordenadas do veículo
            loop(Semaforos, atualizar_veiculo(Veiculos, Veiculo, Coordenadas));
        {parar_veiculo, Veiculo} ->
            %% Para o veículo
            loop(Semaforos, parar_veiculo(Veiculos, Veiculo));
        {mudar_semaforo, Semaforo, Status} ->
            %% Muda o status do semáforo
            loop(atualizar_semaforo(Semaforos, Semaforo, Status), Veiculos)
    end.

%% Função atualizar_veiculo/3
%% Atualiza as coordenadas de um veículo
atualizar_veiculo(Veiculos, Veiculo, Coordenadas) ->
    lists:map(fun(Proc) ->
                    case whereis(Proc) of
                        undefined -> Proc;
                        _ -> if Proc == Veiculo -> Veiculo ! {novas_coordenadas, Coordenadas} end,
                             Proc
                    end
              end, Veiculos).

%% Função parar_veiculo/2
%% Para um veículo
parar_veiculo(Veiculos, Veiculo) ->
    lists:map(fun(Proc) ->
                    case whereis(Proc) of
                        undefined -> Proc;
                        _ -> if Proc == Veiculo -> Veiculo ! {parar_veiculo} end,
                             Proc
                    end
              end, Veiculos).

%% Função atualizar_semaforo/3
%% Atualiza o status de um semáforo
atualizar_semaforo(Semaforos, Semaforo, Status) ->
    lists:map(fun(Proc) ->
                    case whereis(Proc) of
                        undefined -> Proc;
                        _ -> if Proc == Semaforo -> Semaforo ! {mudar_semaforo, Status} end,
                             Proc
                    end
              end, Semaforos).

%% Função semaforo/0
%% Simula o comportamento de um semáforo
semaforo() ->
    loop(verde).

%% Função loop/1
%% Loop principal do semáforo
loop(Status) ->
    receive
        {mudar_semaforo, NovoStatus} ->
            loop(NovoStatus);
        {parar_veiculo} ->
            %% Lógica para parar os veículos
            timer:sleep(100),
            loop(vermelho)
    end.

%% Função veiculo/0
%% Simula o comportamento de um veículo
veiculo() ->
    loop({0, 0}).

%% Função loop/1
%% Loop principal do veículo
loop(Coordenadas) ->
    receive
        {novas_coordenadas, NovasCoordenadas} ->
            %% Lógica para atualizar a posição do veículo
            loop(NovasCoordenadas);
        {parar_veiculo} ->
            %% Lógica para parar o veículo
            timer:sleep(100),
            loop(Coordenadas)
    end.
```

Nesse exemplo, o sistema de controle de tráfego é implementado utilizando processos em Erlang. O módulo `controle_trafego` possui a função `iniciar/0` que inicia o sistema e cria os semáforos e veículos. O controlador de tráfego é responsável por receber mensagens e encaminhá-las para os semáforos e veículos corretos.

Os semáforos são representados por processos individuais, cada um com seu próprio loop que controla o status (verde ou vermelho). Quando o controlador de tráfego recebe uma mensagem para mudar o semáforo, ele repassa essa mensagem para o semáforo correto.

Os veículos também são representados por processos individuais, cada um com seu próprio loop que controla as coordenadas. Quando o controlador de tráfego recebe uma mensagem para atualizar as coordenadas de um veículo, ele repassa essa mensagem para o veículo correto.

Além disso, o controlador de tráfego possui lógica para parar os veículos e atualizar o status dos semáforos, utilizando as funções `atualizar_veiculo/3`, `parar_veiculo/2` e `atualizar_semaforo/3`, respectivamente.

Esse código simula um sistema de controle de tráfego básico, onde os veículos se movimentam e os semáforos podem ser alterados para controlar o fluxo de tráfego. É importante ressaltar que esse é apenas um exemplo de código complexo em Erlang e pode ser adaptado e expandido de acordo com as necessidades específicas do projeto.