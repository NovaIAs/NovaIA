Claro! Vou gerar um código complexo em Prolog para você. Este código é um exemplo de um sistema de recomendação baseado em filtros colaborativos, que utiliza a técnica de vizinhos mais próximos (K-Nearest Neighbors) para encontrar usuários com gostos similares e recomendar itens com base nessas similaridades.

```prolog
% Base de conhecimento com os dados dos usuários
usuario(1, [acao, comedia, aventura, ficcao]).
usuario(2, [romance, drama, comedia]).
usuario(3, [acao, aventura, ficcao]).
usuario(4, [romance, drama]).
usuario(5, [acao, comedia, aventura]).

% Predicado para calcular a similaridade entre dois usuários
similaridade(Usuario1, Usuario2, Sim) :-
    usuario(Usuario1, Filmes1),
    usuario(Usuario2, Filmes2),
    intersecao(Filmes1, Filmes2, Intersecao),
    uniao(Filmes1, Filmes2, Uniao),
    length(Intersecao, NumIntersecao),
    length(Uniao, NumUniao),
    Sim is NumIntersecao / NumUniao.

% Predicado para encontrar os K usuários mais similares a um determinado usuário
k_vizinhos(Usuario, K, Vizinhos) :-
    findall(Sim-User, (usuario(User, _), User \= Usuario, similaridade(Usuario, User, Sim)), Pares),
    keysort(Pares, Sorted),
    reverse(Sorted, Reversed),
    predslice(Reversed, K, Vizinhos).

% Predicado para recomendar filmes para um usuário com base nos seus vizinhos mais similares
recomendar_filmes(Usuario, K, Recomendacao) :-
    k_vizinhos(Usuario, K, Vizinhos),
    findall(Filme, (member(Sim-User, Vizinhos), usuario(User, Filmes), member(Filme, Filmes), \+ member(Filme, FilmesUsuario)), FilmesRecomendados),
    sort(FilmesRecomendados, Recomendacao).

% Predicados auxiliares para operações com listas
uniao([], L, L).
uniao([X|L1], L2, L3) :- member(X, L2), !, uniao(L1, L2, L3).
uniao([X|L1], L2, [X|L3]) :- uniao(L1, L2, L3).

intersecao([], _, []).
intersecao([X|L1], L2, [X|L3]) :- member(X, L2), !, intersecao(L1, L2, L3).
intersecao([_|L1], L2, L3) :- intersecao(L1, L2, L3).

predslice([], _, []).
predslice(_, 0, []).
predslice([X|Xs], N, [X|Ys]) :- N > 0, M is N - 1, predslice(Xs, M, Ys).
```

Explicação do código:

- A base de conhecimento contém informações fictícias sobre os usuários e seus gostos em filmes.
- O predicado `similaridade/3` calcula a similaridade entre dois usuários com base nos filmes que eles assistiram.
- O predicado `k_vizinhos/3` encontra os K usuários mais similares a um determinado usuário, ordenando-os com base na similaridade.
- O predicado `recomendar_filmes/3` utiliza os vizinhos mais similares para recomendar filmes para um usuário específico, excluindo aqueles que o usuário já assistiu.
- Os predicados auxiliares `uniao/3`, `intersecao/3` e `predslice/3` são utilizados para operações com listas, como união, interseção e fatiamento.

Este código é apenas um exemplo de implementação de um sistema de recomendação em Prolog, e pode ser adaptado e personalizado de acordo com as necessidades do projeto.