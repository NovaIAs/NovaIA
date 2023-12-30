Claro! Aqui está um exemplo de código complexo em Prolog:

```
% Base de conhecimento
caminho(a, b, 10).
caminho(a, c, 15).
caminho(b, d, 20).
caminho(b, e, 25).
caminho(c, e, 18).
caminho(d, f, 12).
caminho(e, f, 22).

% Predicados
caminho_valido(X, Y, Distancia) :-
    caminho(X, Y, Distancia).
caminho_valido(X, Y, Distancia) :-
    caminho(Y, X, Distancia).

caminho_minimo(X, Y, Distancia) :-
    findall(D, caminho_valido(X, Y, D), Distancias),
    min_list(Distancias, Distancia).

caminho_mais_curto(X, Y, Distancia, Caminho) :-
    findall((D, C), (caminho_valido(X, Y, D), caminho(X, Y, C)), Distancias),
    min_list(Distancias, (Distancia, Caminho)).

caminho_mais_longo(X, Y, Distancia, Caminho) :-
    findall((D, C), (caminho_valido(X, Y, D), caminho(X, Y, C)), Distancias),
    max_list(Distancias, (Distancia, Caminho)).

% Consultas
?- caminho_minimo(a, f, Distancia).
?- caminho_mais_curto(a, f, Distancia, Caminho).
?- caminho_mais_longo(a, f, Distancia, Caminho).
```

Neste código, temos uma base de conhecimento que representa um grafo com diferentes caminhos entre pontos. Os predicados `caminho_valido/3`, `caminho_minimo/3`, `caminho_mais_curto/4` e `caminho_mais_longo/4` são responsáveis por encontrar o caminho válido mínimo, o caminho mais curto, o caminho mais longo entre dois pontos especificados e retornar a distância percorrida.

Você pode consultar o código para encontrar a distância mínima entre dois pontos (`caminho_minimo/3`), encontrar o caminho mais curto entre dois pontos (`caminho_mais_curto/4`) ou encontrar o caminho mais longo entre dois pontos (`caminho_mais_longo/4`). Basta substituir os pontos de partida e chegada pelos desejados.

Lembre-se de adaptar a base de conhecimento para o seu caso específico, adicionando os caminhos e distâncias necessárias.