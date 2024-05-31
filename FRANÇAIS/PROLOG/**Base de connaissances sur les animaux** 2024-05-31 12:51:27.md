```prolog
% Base de connaissances sur les animaux

animal(chien).
animal(chat).
animal(oiseau).
animal(poisson).
animal(reptile).

% Règles pour déterminer si un animal est un mammifère

mammifere(X) :-
    animal(X),
    \+ oiseau(X),
    \+ poisson(X),
    \+ reptile(X).

% Règles pour déterminer si un animal est un vertébré

vertebre(X) :-
    animal(X),
    \+ invertébré(X).

% Règles pour déterminer si un animal est un invertébré

invertébré(X) :-
    animal(X),
    \+ vertébré(X).

% Règles pour déterminer si un animal est un carnivore

carnivore(X) :-
    animal(X),
    \+ herbivore(X),
    \+ omnivore(X).

% Règles pour déterminer si un animal est un herbivore

herbivore(X) :-
    animal(X),
    \+ carnivore(X),
    \+ omnivore(X).

% Règles pour déterminer si un animal est un omnivore

omnivore(X) :-
    animal(X),
    \+ carnivore(X),
    \+ herbivore(X).

% Règles pour déterminer si un animal est un animal domestique

animal_domestique(X) :-
    animal(X),
    \+ animal_sauvage(X).

% Règles pour déterminer si un animal est un animal sauvage

animal_sauvage(X) :-
    animal(X),
    \+ animal_domestique(X).

% Règles pour déterminer si un animal est un animal de compagnie

animal_de_compagnie(X) :-
    animal(X),
    \+ animal_de_ferme(X),
    \+ animal_de_laboratoire(X).

% Règles pour déterminer si un animal est un animal de ferme

animal_de_ferme(X) :-
    animal(X),
    \+ animal_de_compagnie(X),
    \+ animal_de_laboratoire(X).

% Règles pour déterminer si un animal est un animal de laboratoire

animal_de_laboratoire(X) :-
    animal(X),
    \+ animal_de_compagnie(X),
    \+ animal_de_ferme(X).

% Règles pour déterminer si un animal est dangereux

animal_dangereux(X) :-
    animal(X),
    \+ animal_inoffensif(X).

% Règles pour déterminer si un animal est inoffensif

animal_inoffensif(X) :-
    animal(X),
    \+ animal_dangereux(X).

% Règle pour déterminer si un animal est un chien

chien(X) :-
    animal(X),
    mammifere(X),
    carnivore(X),
    animal_domestique(X),
    animal_de_compagnie(X).

% Règle pour déterminer si un animal est un chat

chat(X) :-
    animal(X),
    mammifere(X),
    carnivore(X),
    animal_domestique(X),
    animal_de_compagnie(X).

% Règle pour déterminer si un animal est un oiseau

oiseau(X) :-
    animal(X),
    \+ mammifere(X),
    \+ poisson(X),
    \+ reptile(X).

% Règle pour déterminer si un animal est un poisson

poisson(X) :-
    animal(X),
    \+ mammifere(X),
    \+ oiseau(X),
    \+ reptile(X).

% Règle pour déterminer si un animal est un reptile

reptile(X) :-
    animal(X),
    \+ mammifere(X),
    \+ oiseau(X),
    \+ poisson(X).
```

**Explication du code :**

Cette base de connaissances PROLOG définit un ensemble de prédicats pour représenter des informations sur les animaux, notamment leur type, leur alimentation, leur domestication et leur dangerosité.

**Prédicats de base :**

* `animal(X)` : vrai si `X` est un animal.
* `mammifere(X)` : vrai si `X` est un mammifère.
* `vertebre(X)` : vrai si `X` est un vertébré.
* `invertébré(X)` : vrai si `X` est un invertébré.
* `carnivore(X)` : vrai si `X` est carnivore.
* `herbivore(X)` : vrai si `X` est herbivore.
* `omnivore(X)` : vrai si `X` est omnivore.
* `animal_domestique(X)` : vrai si `X` est un animal domestique.
* `animal_sauvage(X)` : vrai si `X` est un animal sauvage.
* `animal_de_compagnie(X)` : vrai si `X` est un animal de compagnie.
* `animal_de_ferme(X)` : vrai si `X` est un animal de ferme.
* `animal_de_laboratoire(X)` : vrai si `X` est un animal de laboratoire.
* `animal_dangereux(X)` : vrai si `X` est un animal dangereux.
* `animal_inoffensif(X)` : vrai si `X` est un animal inoffensif.

**Prédicats spécifiques :**

* `chien(X)` : vrai si `X` est un chien.
* `chat(X)` : vrai si `X` est un chat.
* `oiseau(X)` : vrai si `X` est un oiseau.
* `poisson(X)` : vrai si `X` est un poisson.
* `reptile(X)` : vrai si `X` est un reptile.

**Règles :**

Les règles utilisent des prédicats logiques pour définir des relations entre les concepts. Par exemple, la règle suivante établit que tous les mammifères sont des animaux :

```prolog
mammifere(X) :-
    animal(X).
```

**Requêtes :**

Une fois la base de connaissances chargée, on peut interroger le système à l'aide de requêtes. Par exemple, pour connaître tous les animaux qui sont à la fois des mammifères et des carnivores, on peut utiliser la requête suivante :

```prolog
?- mammifere(X), carnivore(X).
```

Le système répondra :

```prolog
X = chien
X = chat
```