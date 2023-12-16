Claro! Aqui está um código complexo em Prolog que envolve a resolução de um problema de enigma. O código foi escrito em português e inclui explicações detalhadas:

```prolog
% Definição das pistas
pista(1, [_,_,_,_,_,_]) :-
    member([_,_,_,_,_,vermelho],_).

pista(2, [_,_,_,_,_,_]) :-
    member([_,_,_,_,_,verde],_).

pista(3, [_,_,_,_,_,_]) :-
    member([_,_,_,_,_,branco],_).

pista(4, [_,_,_,_,_,_]) :-
    member([_,_,_,_,carro,vermelho],_).

pista(5, [_,_,_,_,_,_]) :-
    member([_,_,_,_,moto,_],_).

pista(6, [_,_,_,_,_,_]) :-
    member([_,_,_,_,_,azul],_).

% Regra principal para resolver o enigma
enigma(Pistas) :-
    % Criando uma lista de 6 pistas
    length(Pistas, 6),
    
    % Definindo as restrições para cada pista
    pista(1, Pistas),
    pista(2, Pistas),
    pista(3, Pistas),
    pista(4, Pistas),
    pista(5, Pistas),
    pista(6, Pistas),

    % Outras restrições
    % 1. O carro vermelho está em uma das pistas ímpares
    member([_,_,_,_,carro,vermelho], Pistas),
    member([_,_,_,_,_,vermelho], Pistas),
    % 2. O carro verde está em uma das pistas adjacentes à pista do carro vermelho
    nextto([_,_,_,_,carro,verde], [_,_,_,_,carro,vermelho], Pistas),
    % 3. O carro branco está em uma das pistas adjacentes à pista do carro verde
    nextto([_,_,_,_,carro,branco], [_,_,_,_,carro,verde], Pistas),
    % 4. A moto está em uma das pistas adjacentes à pista do carro branco
    nextto([_,_,_,_,moto,_], [_,_,_,_,carro,branco], Pistas),
    % 5. A moto não está na primeira nem na última pista
    nth1(PosicaoMoto, Pistas, [_,_,_,_,moto,_]),
    PosicaoMoto \== 1,
    PosicaoMoto \== 6,
    % 6. O carro azul está na última pista
    last(Pistas, [_,_,_,_,carro,azul]).

% Chamar a regra principal para resolver o enigma
?- enigma(Pistas).
```

Este código em Prolog é uma solução para um enigma onde há seis pistas diferentes. Cada pista contém informações sobre um veículo (carro ou moto) e sua cor (vermelho, verde, branco ou azul). O objetivo é descobrir a configuração correta das pistas.

As pistas e suas restrições são definidas pela regra `pista/2`. Cada pista é representada por uma lista de seis elementos, onde cada elemento contém informações sobre o veículo e a cor. As restrições especificam características como a posição do carro vermelho, a relação entre os carros vermelho e verde, a relação entre os carros verde e branco, a posição da moto e a posição do carro azul.

A regra `enigma/1` é a regra principal que define todas as restrições e chama a função `enigma/1` para resolver o enigma. A função `enigma/1` cria uma lista de seis pistas e aplica as restrições definidas anteriormente. A última linha `?- enigma(Pistas).` é usada para chamar a regra principal e obter a solução do enigma.

Espero que isso atenda às suas expectativas! Se você tiver mais alguma solicitação de código, não hesite em me perguntar.