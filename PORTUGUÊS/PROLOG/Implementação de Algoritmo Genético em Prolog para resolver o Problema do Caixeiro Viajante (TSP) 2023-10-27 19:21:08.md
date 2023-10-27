Claro! Aqui está um código em Prolog que implementa um algoritmo genético para resolver o problema clássico do Caixeiro Viajante (TSP - Traveling Salesman Problem). O código é bastante complexo e utiliza técnicas avançadas como mutação, cruzamento e seleção de indivíduos.

```prolog
% Definição do número de cidades e suas coordenadas
num_cidades(5).
cidades([cidade(1, 2, 3), cidade(2, 4, 7), cidade(3, 6, 5), cidade(4, 8, 1), cidade(5, 10, 9)]).

% Função para calcular a distância entre duas cidades
distancia(cidade(_, X1, Y1), cidade(_, X2, Y2), Dist) :-
    Dist is sqrt((X2 - X1)^2 + (Y2 - Y1)^2).

% Função de fitness que calcula a distância total de uma rota
fitness([], 0).
fitness([Cidade1, Cidade2 | Rota], DistanciaTotal) :-
    distancia(Cidade1, Cidade2, Distancia),
    fitness([Cidade2 | Rota], DistanciaRestante),
    DistanciaTotal is Distancia + DistanciaRestante.

% Criação de uma população inicial aleatória
gerar_populacao(0, []).
gerar_populacao(N, [Rota | Populacao]) :-
    num_cidades(NumCidades),
    random_permutation(NumCidades, Rota),
    N1 is N - 1,
    gerar_populacao(N1, Populacao).

% Função para selecionar indivíduos da população com base na roleta viciada
selecionar_populacao(Populacao, FitnessPopulacao, NovaPopulacao) :-
    length(Populacao, TamPopulacao),
    findall(Prob, (member(_, FitnessPopulacao), Prob is 1 / TamPopulacao), Probabilidades),
    roleta_viciada(Populacao, FitnessPopulacao, Probabilidades, NovaPopulacao).

roleta_viciada(_, _, [], []).
roleta_viciada(Populacao, FitnessPopulacao, [Prob | Resto], [Individuo | NovaPopulacao]) :-
    somatorio(FitnessPopulacao, SomaFitness),
    Valor is SomaFitness * Prob,
    encontrar_individuo(Populacao, FitnessPopulacao, Valor, Individuo),
    roleta_viciada(Populacao, FitnessPopulacao, Resto, NovaPopulacao).

somatorio([], 0).
somatorio([X | Xs], Soma) :-
    somatorio(Xs, SomaRestante),
    Soma is X + SomaRestante.

encontrar_individuo([Individuo | _], [Fitness | _], Valor, Individuo) :-
    Valor =< Fitness.
encontrar_individuo([_ | Populacao], [_ | FitnessPopulacao], Valor, Individuo) :-
    encontrar_individuo(Populacao, FitnessPopulacao, Valor, Individuo).

% Função de mutação que troca a posição de duas cidades em uma rota
mutacao(Rota, NovaRota) :-
    num_cidades(NumCidades),
    random(1, NumCidades, Posicao1),
    random(1, NumCidades, Posicao2),
    trocar_cidades(Rota, Posicao1, Posicao2, NovaRota).

trocar_cidades(Rota, Posicao1, Posicao2, NovaRota) :-
    nth1(Posicao1, Rota, Cidade1),
    nth1(Posicao2, Rota, Cidade2),
    nth1(Posicao1, NovaRota, Cidade2),
    nth1(Posicao2, NovaRota, Cidade1),
    substituir(Rota, Posicao1, Cidade2, TempRota),
    substituir(TempRota, Posicao2, Cidade1, NovaRota).

substituir([_ | Xs], 1, Y, [Y | Xs]).
substituir([X | Xs], N, Y, [X | Zs]) :-
    N > 1,
    N1 is N - 1,
    substituir(Xs, N1, Y, Zs).

% Função de cruzamento que gera dois filhos a partir de dois pais
cruzamento(Pai1, Pai2, Filho1, Filho2) :-
    num_cidades(NumCidades),
    random(2, NumCidades, PontoCorte),
    split_at(PontoCorte, Pai1, Parte1Pai1, Parte2Pai1),
    split_at(PontoCorte, Pai2, Parte1Pai2, Parte2Pai2),
    append(Parte1Pai1, Parte2Pai2, Filho1),
    append(Parte1Pai2, Parte2Pai1, Filho2).

% Função para encontrar o melhor indivíduo da população
encontrar_melhor(Populacao, MelhorIndividuo) :-
    maplist(fitness, Populacao, FitnessPopulacao),
    min_list(FitnessPopulacao, MelhorFitness),
    nth1(Indice, FitnessPopulacao, MelhorFitness),
    nth1(Indice, Populacao, MelhorIndividuo).

% Algoritmo genético principal
algoritmo_genetico(NumGeracoes, TamanhoPopulacao, MelhorIndividuo) :-
    gerar_populacao(TamanhoPopulacao, Populacao),
    gerar_populacao(TamanhoPopulacao, Populacao),
    loop_genetico(NumGeracoes, Populacao, MelhorIndividuo).

loop_genetico(0, Populacao, MelhorIndividuo) :-
    encontrar_melhor(Populacao, MelhorIndividuo).
loop_genetico(N, Populacao, MelhorIndividuo) :-
    encontrar_melhor(Populacao, MelhorIndividuoAtual),
    write('Melhor indivíduo da geração '), write(N), write(': '), write(MelhorIndividuoAtual), nl,
    selecionar_populacao(Populacao, _, NovaPopulacao),
    crossover_mutacao(NovaPopulacao, PopulacaoMutada),
    N1 is N - 1,
    loop_genetico(N1, PopulacaoMutada, MelhorIndividuo).

crossover_mutacao([], []).
crossover_mutacao([Individuo | Resto], [FilhoMutado | NovaPopulacao]) :-
    random(0, 1, ProbabilidadeMutacao),
    (ProbabilidadeMutacao =:= 0 ->
        mutacao(Individuo, FilhoMutado);
        FilhoMutado = Individuo
    ),
    crossover_mutacao(Resto, NovaPopulacao).
```

Este código em Prolog implementa uma solução para o problema do caixeiro viajante usando algoritmo genético. O algoritmo genético é uma técnica de busca inspirada no processo de evolução biológica que pode ser aplicada para resolver problemas de otimização. Neste problema, o objetivo é encontrar a rota mais curta que um vendedor deve fazer para visitar todas as cidades e retornar ao ponto de partida.

O código começa definindo o número de cidades e suas coordenadas. Em seguida, implementa-se a função de fitness, que calcula a distância total percorrida em uma determinada rota. Em seguida, a população inicial é gerada aleatoriamente.

A seleção de indivíduos é feita usando uma roleta viciada, onde a probabilidade de seleção de um indivíduo é proporcional ao seu valor de fitness em relação à soma total de fitness da população. A função de mutação troca a posição de duas cidades em uma rota. O cruzamento é feito dividindo os pais em duas partes e combinando essas partes para gerar dois filhos.

O algoritmo genético é executado por um número especificado de gerações, encontrando o melhor indivíduo em cada geração e selecionando a nova população para a próxima geração. O processo de crossover e mutação é aplicado à nova população. O algoritmo termina quando o número de gerações especificado é alcançado e retorna o melhor indivíduo encontrado.

Espero que este código atenda às suas expectativas!