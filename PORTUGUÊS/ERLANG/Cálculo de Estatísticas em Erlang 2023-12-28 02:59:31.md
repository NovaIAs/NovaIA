Claro! Aqui está um exemplo de um código complexo em Erlang que envolve o processamento de uma lista de números e a geração de estatísticas sobre eles:

```erlang
-module(estatisticas).

-export([calcular_estatisticas/1]).

calcular_estatisticas(Lista) ->
    {Min, Max, Media, Soma} = processar_lista(Lista),
    io:format("Estatísticas da lista:~n"),
    io:format("Menor valor: ~p~n", [Min]),
    io:format("Maior valor: ~p~n", [Max]),
    io:format("Média: ~p~n", [Media]),
    io:format("Soma: ~p~n", [Soma]).

processar_lista(Lista) ->
    {Min, Max, Soma, Contador} = processar_lista(Lista, infinity, -infinity, 0, 0),
    Media = Soma / Contador,
    {Min, Max, Media, Soma}.

processar_lista([], Min, Max, Soma, Contador) ->
    {Min, Max, Soma, Contador};
processar_lista([H|T], Min, Max, Soma, Contador) ->
    NovoMin = if
        H < Min -> H;
        true -> Min
    end,
    NovoMax = if
        H > Max -> H;
        true -> Max
    end,
    NovaSoma = Soma + H,
    NovoContador = Contador + 1,
    processar_lista(T, NovoMin, NovoMax, NovaSoma, NovoContador).
```

Neste código, o módulo `estatisticas` contém uma função `calcular_estatisticas/1` que recebe uma lista de números como entrada e calcula a menor, maior, média e soma dos valores.

A função `calcular_estatisticas/1` chama a função `processar_lista/1`, que realiza o processamento real da lista. A função `processar_lista/1` utiliza recursão para iterar sobre a lista, atualizando as variáveis `Min`, `Max`, `Soma` e `Contador` a cada iteração.

No final do processamento, as estatísticas são exibidas na saída padrão usando a função `io:format/2`. O resultado é exibido no seguinte formato:

```
Estatísticas da lista:
Menor valor: X
Maior valor: Y
Média: Z
Soma: W
```

Substitua `X`, `Y`, `Z` e `W` pelos valores reais calculados.

Espero que este código complexo em Erlang atenda às suas expectativas!