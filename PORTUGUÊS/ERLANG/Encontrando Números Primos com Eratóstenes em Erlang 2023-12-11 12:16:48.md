Claro! Aqui está um código complexo em ERLANG que envolve o cálculo de números primos usando o Crivo de Eratóstenes. O código está em português e inclui comentários explicativos para facilitar a compreensão:

```erlang
-module(primeiro).
-export([crivo/1]).

%% Função principal que recebe um número limite e retorna uma lista
%% com todos os números primos até esse limite.
crivo(N) ->
    %% Inicializa uma lista de números de 2 até N
    Lista = lists:seq(2, N),
    %% Chama a função auxiliar passando a lista e um número inicial
    crivo_aux(Lista, 2).

%% Função auxiliar que recebe uma lista e um número e retorna a lista
%% atualizada com os números não primos removidos.
crivo_aux([], _) ->
    []; %% Caso base: a lista está vazia, retorna uma lista vazia.
crivo_aux([H|T], P) when H rem P == 0 ->
    %% Se o número H é divisível por P, remove H da lista
    crivo_aux(T, P);
crivo_aux([H|T], P) ->
    %% Caso contrário, mantém o número H na lista e chama a função
    %% recursivamente com o próximo número primo.
    [H | crivo_aux(T, proximo_primo([H|T], P))].

%% Função que retorna o próximo número primo maior que o número P.
proximo_primo(Lista, P) ->
    case lists:dropwhile(fun(X) -> X =< P end, Lista) of
        [Proximo|_] -> Proximo;
        _ -> P
    end.
```

Este código implementa o algoritmo do Crivo de Eratóstenes para encontrar números primos em um intervalo até um número limite N. Ele começa criando uma lista de números de 2 até N. Em seguida, a função auxiliar `crivo_aux/2` percorre a lista e remove os números que são múltiplos do número inicial P (que começa como 2). Isso é feito através de uma chamada recursiva da função `crivo_aux/2`, passando a lista atualizada e o próximo número primo após P.

A função `proximo_primo/2` é responsável por encontrar o próximo número primo maior que o número P. Ela utiliza a função `lists:dropwhile/2` para descartar todos os números menores ou iguais a P da lista e retorna o próximo número encontrado.

Por fim, a função principal `crivo/1` recebe o número limite N como argumento, inicializa a lista de números de 2 até N e chama a função auxiliar `crivo_aux/2` com essa lista e o número inicial 2. O resultado final é a lista com todos os números primos até o limite especificado.

Espero que este código atenda às suas expectativas!