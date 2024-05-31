**Programme de calcul de la factorielle d'un nombre**

```smalltalk
"Faculté d'un nombre"

nombre := 5.
"Initialisation de la variable 'nombre' à 5"

factorielle := 1.
"Initialisation de la variable 'factorielle' à 1"

1 to: nombre do: [:i | factorielle := factorielle * i].
"Boucle qui calcule la factorielle du nombre 'nombre'"

Transcript show: factorielle.
"Affichage du résultat dans la console"
```

**Explication du code**

* `nombre := 5.` : Assignation de la valeur 5 à la variable `nombre`.
* `factorielle := 1.` : Assignation de la valeur 1 à la variable `factorielle`.
* `1 to: nombre do: [:i | factorielle := factorielle * i].` : Boucle qui itère de 1 à `nombre`. Pour chaque entier `i` dans cette plage, la valeur de `factorielle` est multipliée par `i`. Cette boucle calcule la factorielle de `nombre`.
* `Transcript show: factorielle.` : Affichage de la valeur de `factorielle` dans la console à l'aide de la classe `Transcript`.

**Code de tri d'une liste d'entiers**

```smalltalk
"Tri d'une liste d'entiers"

liste := {5, 3, 1, 2, 4}.
"Initialisation de la variable 'liste' avec une liste d'entiers"

liste sort.
"Tri de la liste 'liste' en ordre croissant"

Transcript show: liste.
"Affichage de la liste triée dans la console"
```

**Explication du code**

* `liste := {5, 3, 1, 2, 4}.` : Assignation d'une liste d'entiers à la variable `liste`.
* `liste sort.` : Tri de la liste `liste` en ordre croissant à l'aide de la méthode `sort`.
* `Transcript show: liste.` : Affichage de la liste triée dans la console.

**Code de création d'un dictionnaire de noms et de numéros de téléphone**

```smalltalk
"Dictionnaire nom-numéro de téléphone"

annuaire := Dictionary new.
"Création d'un nouveau dictionnaire nommé 'annuaire'"

annuaire at: 'Jean' put: '123-456-7890'.
"Insertion du nom 'Jean' avec le numéro '123-456-7890' dans le dictionnaire"

annuaire at: 'Marie' put: '123-456-7891'.
"Insertion du nom 'Marie' avec le numéro '123-456-7891' dans le dictionnaire"

Transcript show: annuaire.
"Affichage du dictionnaire dans la console"
```

**Explication du code**

* `annuaire := Dictionary new.` : Création d'un nouveau dictionnaire nommé `annuaire`.
* `annuaire at: 'Jean' put: '123-456-7890'.` : Insertion du nom `Jean` avec le numéro `123-456-7890` dans le dictionnaire à l'aide de la méthode `at:put:`.
* `annuaire at: 'Marie' put: '123-456-7891'.` : Insertion du nom `Marie` avec le numéro `123-456-7891` dans le dictionnaire.
* `Transcript show: annuaire.` : Affichage du dictionnaire dans la console.

Ce code crée un dictionnaire avec deux entrées, chacune associant un nom à un numéro de téléphone.