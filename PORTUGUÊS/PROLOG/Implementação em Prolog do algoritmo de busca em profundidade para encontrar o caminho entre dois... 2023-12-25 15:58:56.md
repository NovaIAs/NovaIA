Claro! Aqui está um código em Prolog que implementa o algoritmo de busca em profundidade para encontrar o caminho entre dois pontos em um labirinto representado por uma matriz:

```prolog
% Predicado principal para encontrar o caminho no labirinto
encontrar_caminho(Labirinto, Inicio, Destino, Caminho) :-
    dfs(Labirinto, Inicio, Destino, [Inicio], Caminho).

% Predicado de busca em profundidade
dfs(_, Destino, Destino, Caminho, Caminho).
dfs(Labirinto, Atual, Destino, CaminhoAtual, CaminhoFinal) :-
    vizinho(Labirinto, Atual, Proximo),
    \+ member(Proximo, CaminhoAtual),
    dfs(Labirinto, Proximo, Destino, [Proximo|CaminhoAtual], CaminhoFinal).

% Predicado para encontrar um vizinho válido
vizinho(Labirinto, (X, Y), (X2, Y)) :-
    X2 is X + 1,
    dentro_limites(Labirinto, (X2, Y)).

vizinho(Labirinto, (X, Y), (X, Y2)) :-
    Y2 is Y + 1,
    dentro_limites(Labirinto, (X, Y2)).

vizinho(Labirinto, (X, Y), (X2, Y)) :-
    X2 is X - 1,
    dentro_limites(Labirinto, (X2, Y)).

vizinho(Labirinto, (X, Y), (X, Y2)) :-
    Y2 is Y - 1,
    dentro_limites(Labirinto, (X, Y2)).

% Predicado para verificar se um ponto está dentro dos limites do labirinto
dentro_limites(Labirinto, (X, Y)) :-
    length(Labirinto, Linhas),
    length(Labirinto, Colunas),
    X >= 0,
    X < Linhas,
    Y >= 0,
    Y < Colunas,
    nth0(X, Labirinto, Linha),
    nth0(Y, Linha, Espaco),
    Espaco \== parede.
```

Neste código, temos o predicado principal `encontrar_caminho/4` que recebe o labirinto representado por uma matriz, as coordenadas de início e destino, e retorna o caminho entre esses dois pontos.

O predicado `dfs/5` é responsável pela busca em profundidade. Ele verifica se a posição atual é o destino final. Caso seja, ele retorna o caminho atual como o caminho final. Caso contrário, ele encontra um vizinho não visitado, adiciona-o ao caminho atual e continua a busca recursivamente.

O predicado `vizinho/3` encontra um vizinho válido dado uma posição no labirinto. Ele verifica se a próxima posição está dentro dos limites do labirinto e se não é uma parede.

Por fim, o predicado `dentro_limites/2` verifica se uma posição está dentro dos limites do labirinto, verificando se as coordenadas X e Y estão dentro do intervalo válido e se o espaço correspondente não é uma parede.

Espero que este código atenda às suas expectativas!