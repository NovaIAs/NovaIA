Claro! Aqui está um código complexo em PROLOG:

```prolog
% Definição das relações

% Relação pai
pai(joao, maria).
pai(joao, pedro).
pai(pedro, lucas).
pai(pedro, ana).
pai(pedro, gabriel).
pai(pedro, daniel).
pai(pedro, sofia).

% Relação mãe
mae(maria, lucas).
mae(maria, ana).
mae(maria, gabriel).
mae(maria, daniel).
mae(maria, sofia).
mae(ana, joana).
mae(joana, rafaela).

% Relação masculino
masculino(joao).
masculino(pedro).
masculino(lucas).
masculino(gabriel).
masculino(daniel).

% Relação feminino
feminino(maria).
feminino(ana).
feminino(sofia).
feminino(joana).
feminino(rafaela).

% Definição de regras/fatos complexos

% Regra: avô
avo(Avo, Neto) :-
    pai(Avo, Filho),
    (pai(Filho, Neto) ; mae(Filho, Neto)),
    masculino(Avo).

% Regra: avó
avo(Avo, Neto) :-
    mae(Avo, Filho),
    (pai(Filho, Neto) ; mae(Filho, Neto)),
    feminino(Avo).

% Regra: tio
tio(Tio, Sobrinho) :-
    pai(Pai, Sobrinho),
    (irmao(Pai, Tio) ; irma(Pai, Tio)),
    masculino(Tio).

% Regra: tia
tia(Tia, Sobrinho) :-
    mae(Mae, Sobrinho),
    (irmao(Mae, Tia) ; irma(Mae, Tia)),
    feminino(Tia).

% Regra: irmão
irmao(Irmao, Pessoa) :-
    pai(Pai, Pessoa),
    pai(Pai, Irmao),
    Pessoa \= Irmao,
    masculino(Irmao).

% Regra: irmã
irma(Irma, Pessoa) :-
    mae(Mae, Pessoa),
    mae(Mae, Irma),
    Pessoa \= Irma,
    feminino(Irma).
```

Este código em PROLOG representa um sistema de genealogia. Ele possui uma série de relações que definem pais, mães, gênero e regras complexas para determinar avós, tios, irmãos e irmãs.

As relações são definidas através de fatos. Por exemplo, a relação "pai(joao, maria)" afirma que "joao" é pai de "maria". Existem também as relações "mae", "masculino" e "feminino", que seguem a mesma lógica.

As regras complexas são definidas utilizando combinações de relações. Por exemplo, a regra "avo(Avo, Neto)" afirma que "Avo" é avô de "Neto" se existir um "Filho" que é pai ou mãe de "Neto" e "Avo" é pai deste "Filho", além de ser do gênero masculino.

Outras regras como "tio", "tia", "irmao" e "irma" seguem a mesma lógica, utilizando as relações existentes para determinar os parentescos.

Espero que este código atenda às suas expectativas!