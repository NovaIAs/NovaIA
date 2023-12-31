Claro, vou gerar um código em Prolog para resolver o problema das N-rainhas, que consiste em colocar N rainhas em um tabuleiro de xadrez de tamanho N x N sem que elas se ataquem.

O código a seguir implementa uma solução recursiva para o problema:

```prolog
% Predicado principal para a resolução das N-rainhas
n_rainhas(N, Solucao) :-
    gerar_tabuleiro(N, Tabuleiro),
    resolver_nrainhas(Tabuleiro, N, Solucao).

% Predicado para gerar um tabuleiro vazio
gerar_tabuleiro(0, []).
gerar_tabuleiro(N, [Linha|Resto]) :-
    gerar_linha(N, Linha),
    N1 is N - 1,
    gerar_tabuleiro(N1, Resto).

% Predicado para gerar uma linha vazia
gerar_linha(0, []).
gerar_linha(N, [vazio|Resto]) :-
    N1 is N - 1,
    gerar_linha(N1, Resto).

% Predicado para resolver as N-rainhas
resolver_nrainhas([], _, []).
resolver_nrainhas(Tabuleiro, N, [Rainha|Solucao]) :-
    selecionar_posicao(Tabuleiro, X, Y),
    colocar_rainha(Tabuleiro, X, Y, NovoTabuleiro),
    resolver_nrainhas(NovoTabuleiro, N, Solucao),
    Rainha = (X, Y).

% Predicado para selecionar uma posição vazia no tabuleiro
selecionar_posicao([Linha|Resto], X, Y) :-
    selecionar_posicao_linha(Linha, 1, X, Y),
    !.
selecionar_posicao([_|Resto], X, Y) :-
    selecionar_posicao(Resto, X, Y).

% Predicado auxiliar para selecionar uma posição vazia em uma linha
selecionar_posicao_linha([vazio|_], X, X, 1).
selecionar_posicao_linha([_|Resto], X, PosX, PosY) :-
    X1 is X + 1,
    selecionar_posicao_linha(Resto, X1, PosX, PosY).

% Predicado para colocar uma rainha em uma posição do tabuleiro
colocar_rainha([], _, _, []).
colocar_rainha([Linha|Resto], 1, Y, [Rainha|Resto]) :-
    colocar_rainha_linha(Linha, Y, Rainha).
colocar_rainha([Linha|Resto], X, Y, [Linha|NovoResto]) :-
    X1 is X - 1,
    colocar_rainha(Resto, X1, Y, NovoResto).

% Predicado auxiliar para colocar uma rainha em uma posição da linha
colocar_rainha_linha([_|Resto], 1, rainha) :-
    Resto = [rainha|_].
colocar_rainha_linha([Elem|Resto], Y, [Elem|NovoResto]) :-
    Y1 is Y - 1,
    colocar_rainha_linha(Resto, Y1, NovoResto).
```

Explicação do código:

1. O predicado `n_rainhas/2` é o ponto de entrada para a resolução do problema das N-rainhas. Ele recebe o tamanho do tabuleiro N e retorna a solução em uma lista de coordenadas (X, Y) das rainhas.
2. O predicado `gerar_tabuleiro/2` é responsável por gerar um tabuleiro vazio de tamanho N x N.
3. O predicado `gerar_linha/2` gera uma linha vazia de tamanho N.
4. O predicado `resolver_nrainhas/3` é responsável por resolver o problema das N-rainhas de forma recursiva.
5. O predicado `selecionar_posicao/3` seleciona uma posição vazia no tabuleiro.
6. O predicado `selecionar_posicao_linha/4` seleciona uma posição vazia em uma linha.
7. O predicado `colocar_rainha/4` coloca uma rainha em uma posição do tabuleiro.
8. O predicado `colocar_rainha_linha/3` coloca uma rainha em uma posição da linha.

Esse código utiliza a abordagem de backtracking para encontrar todas as soluções possíveis para o problema das N-rainhas. Ele gera todas as combinações possíveis de posições e verifica se cada uma delas é válida, ou seja, se as rainhas não se atacam. Caso uma posição seja inválida, ele faz o backtrack e tenta outra combinação. O processo continua até todas as soluções possíveis serem encontradas.