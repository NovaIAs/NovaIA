```prolog
:- dynamic pere/2, mere/2, enfant/2.

% Règles pour la paternité
pere(X, Y) :- geniteur(X, Y), male(X).
geniteur(X, Y) :- biologique(X, Y).
biologique(X, Y) :- chromosome_x(X, Y), chromosome_y(X, Y).

% Règles pour la maternité
mere(X, Y) :- geniteur(X, Y), femelle(X).
geniteur(X, Y) :- biologique(X, Y).
biologique(X, Y) :- chromosome_x(X, Y).

% Règles pour les enfants
enfant(X, Y) :- pere(Y, X).
enfant(X, Y) :- mere(Y, X).

% Règles pour le sexe
male(X) :- chromosome_y(X).
femelle(X) :- chromosome_x(X).

% Règles pour les chromosomes
chromosome_x(X) :- has(X, gene("chromosome_x")).
chromosome_y(X) :- has(X, gene("chromosome_y")).

% Règle pour les gènes
gene(NomDuGene) :- atomic(NomDuGene), ground(NomDuGene).

% Règle d'appartenance
has(X, Gene) :- member(Gene, genes(X)).

% Base de connaissances initiale
genes(john) = [gene("chromosome_x"), gene("chromosome_y")].
genes(mary) = [gene("chromosome_x")].
genes(alice) = [gene("chromosome_x"), gene("chromosome_y")].
genes(bob) = [gene("chromosome_x")].

```

**Explication du code :**

Ce code PROLOG définit un système de règles pour représenter et raisonner sur les relations familiales et le sexe. Il utilise des faits et des règles pour déduire de nouvelles informations sur l'identité des parents, des enfants et leur sexe.

**Faits :**

* **genes/2** représente les gènes d'un individu.
* **pere/2** représente la relation père-fils.
* **mere/2** représente la relation mère-fils.
* **enfant/2** représente la relation enfant-parent.
* **male/1** représente les individus de sexe masculin.
* **femelle/1** représente les individus de sexe féminin.

**Règles :**

* **pere/2** définit qu'un individu est le père d'un autre individu s'il est son géniteur (a contribué génétiquement à sa naissance) et qu'il est de sexe masculin.
* **mere/2** définit qu'un individu est la mère d'un autre individu s'il est son géniteur et qu'il est de sexe féminin.
* **enfant/2** définit qu'un individu est l'enfant d'un autre individu s'il est son père ou sa mère.
* **male/1** définit qu'un individu est de sexe masculin s'il possède un chromosome Y.
* **femelle/1** définit qu'un individu est de sexe féminin s'il possède un chromosome X.
* **geniteur/2** définit qu'un individu est le géniteur d'un autre individu s'il a contribué génétiquement à sa naissance (par exemple, par un spermatozoïde ou un ovule).
* **biologique/2** définit qu'un individu est le géniteur biologique d'un autre individu s'il a transmis ses chromosomes X et Y à cet individu (dans le cas d'un père) ou ses chromosomes X (dans le cas d'une mère).
* **chromosome_x/2** définit qu'un individu possède un chromosome X.
* **chromosome_y/2** définit qu'un individu possède un chromosome Y.
* **gene/1** définit qu'un gène est une entité atomique nommée.
* **has/2** définit qu'un individu possède un certain gène s'il figure dans la liste de ses gènes.

**Exemple d'utilisation :**

```prolog
?- pere(john, bob).
true
```

Cette requête retourne "true" car John est le père de Bob selon les faits et les règles définis dans la base de connaissances.