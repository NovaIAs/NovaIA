```prolog
% Base de connaissances

% Faits
velo(bleu).
velo(rouge).
velo(vert).

roue(24).
roue(26).
roue(29).

couleur_roue(bleu, 24).
couleur_roue(vert, 24).
couleur_roue(rouge, 26).
couleur_roue(vert, 29).
couleur_roue(bleu, 29).

% Règles
velo_avec_roue(CouleurVelo, TailleRoue) :-
    velo(CouleurVelo),
    roue(TailleRoue),
    couleur_roue(CouleurVelo, TailleRoue).

% Requête
tous_les_velos_rouges_avec_roues_24 :-
    findall(CouleurVelo, velo_avec_roue(CouleurVelo, 24), VelosRouges24),
    write(VelosRouges24).
```

**Explication du code**

Ce code Prolog définit une base de connaissances sur les vélos et leurs caractéristiques, telles que la couleur et la taille des roues.

**Faits**

Les faits sont des déclarations qui décrivent des informations statiques. Dans ce code, les faits décrivent :

* Les couleurs disponibles pour les vélos : `velo(Couleur)`
* Les tailles disponibles pour les roues : `roue(Taille)`
* Les combinaisons de couleurs et de tailles de roues disponibles : `couleur_roue(Couleur, Taille)`

**Règles**

Les règles sont des clauses qui définissent des relations entre les faits. Dans ce code, la règle suivante définit la relation `velo_avec_roue` :

```prolog
velo_avec_roue(CouleurVelo, TailleRoue) :-
    velo(CouleurVelo),
    roue(TailleRoue),
    couleur_roue(CouleurVelo, TailleRoue).
```

Cette règle indique qu'un vélo a une roue si le vélo existe, la roue existe et la combinaison couleur-taille de la roue est disponible.

**Requête**

La requête suivante récupère tous les vélos rouges avec des roues de 24 pouces :

```prolog
tous_les_velos_rouges_avec_roues_24 :-
    findall(CouleurVelo, velo_avec_roue(CouleurVelo, 24), VelosRouges24),
    write(VelosRouges24).
```

Cette requête utilise la fonction `findall` pour collecter tous les vélos rouges avec des roues de 24 pouces dans la liste `VelosRouges24`, puis elle affiche la liste.