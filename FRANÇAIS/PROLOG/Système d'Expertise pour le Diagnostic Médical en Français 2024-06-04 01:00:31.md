**Système d'expertise pour le diagnostic médical en français**

```prolog
% Base de connaissances
regle(mal_de_tete, grippe).
regle(fievre, grippe).
regle(toux, grippe).
regle(nez_qui_coule, rhume).
regle(eternuements, rhume).
regle(mal_de_gorge, angine).

% Faits observés
fait(mal_de_tete).
fait(fievre).
fait(toux).

% Moteur d'inférence
diagnostique(Maladie) :-
    fait(Symptome),
    regle(Symptome, Maladie),
    not(deja_trouve(Maladie)),
    assert(deja_trouve(Maladie)).

% Boucle principale
main :-
    repeat,
    write("Entrez un symptôme (ou fin pour quitter) : "),
    read(Symptome),
    Symptome \= fin,
    assert(fait(Symptome)),
    diagnostique(Maladie),
    write("Diagnostic : "),
    write(Maladie),
    nl.
```

**Explication**

Ce code PROLOG implémente un système d'expertise simple pour le diagnostic médical.

* La **base de connaissances** est représentée par une liste de règles de la forme `regle(Symptome, Maladie)`, où `Symptome` est un symptôme et `Maladie` est la maladie associée.
* Les **faits observés** sont des symptômes signalés par le patient.
* Le **moteur d'inférence** utilise des règles et des faits pour déduire des maladies possibles.
* La fonction `diagnostique/1` déduit une maladie à partir d'un symptôme observé.
* La fonction `main/0` est la boucle principale du programme, qui demande des symptômes à l'utilisateur jusqu'à ce qu'il entre "fin".

**Exemple**

Si l'utilisateur entre les symptômes suivants :

```
mal_de_tete
fievre
toux
```

Le programme déduira le diagnostic suivant :

```
Diagnostic : grippe
```