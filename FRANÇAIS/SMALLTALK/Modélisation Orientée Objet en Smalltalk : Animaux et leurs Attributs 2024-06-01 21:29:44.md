**Classe `Animal`**

```smalltalk
Classe Animal
  instanceVariableNames: 'nom age'

  class
    méthode: 'nouvelleAvecNom:'
      | unNom |
      unNom := unNom.
      ^ self nouveauAvecNom: unNom age: 1

    méthode: 'nouveauAvecNom:age:'
      | unNom unAge |
      unNom := unNom.
      unAge := unAge.
      super nouveau.
      nom := unNom.
      age := unAge.
      ^ self

  méthodesInstance
    méthode: 'nom'
      ^ nom

    méthode: 'changerNom:'
      | unNom |
      unNom := unNom.
      nom := unNom

    méthode: 'age'
      ^ age

    méthode: 'changerAge:'
      | unAge |
      unAge := unAge.
      age := unAge
```

**Classe `Chat`**

```smalltalk
Classe Chat héritantDe: Animal
  instanceVariableNames: 'race'

  class
    méthode: 'nouvelleAvecRace:'
      | uneRace |
      uneRace := uneRace.
      ^ self nouveauAvecNom: '' age: 1 race: uneRace

    méthode: 'nouveauAvecNom:age:race:'
      | unNom unAge uneRace |
      unNom := unNom.
      unAge := unAge.
      uneRace := uneRace.
      super nouveauAvecNom: unNom age: unAge.
      race := uneRace
      ^ self

  méthodesInstance
    méthode: 'race'
      ^ race

    méthode: 'changerRace:'
      | uneRace |
      uneRace := uneRace.
      race := uneRace
```

**Classe `Chien`**

```smalltalk
Classe Chien héritantDe: Animal
  instanceVariableNames: 'race'

  class
    méthode: 'nouvelleAvecRace:'
      | uneRace |
      uneRace := uneRace.
      ^ self nouveauAvecNom: '' age: 1 race: uneRace

    méthode: 'nouveauAvecNom:age:race:'
      | unNom unAge uneRace |
      unNom := unNom.
      unAge := unAge.
      uneRace := uneRace.
      super nouveauAvecNom: unNom age: unAge.
      race := uneRace
      ^ self

  méthodesInstance
    méthode: 'race'
      ^ race

    méthode: 'changerRace:'
      | uneRace |
      uneRace := uneRace.
      race := uneRace
```

**Utilisation des classes**

```smalltalk
| unChat unChien |
unChat := Chat nouvelleAvecRace: 'Persan'.
unChien := Chien nouvelleAvecRace: 'Labrador'.

". Nom du chat : " écrir.
unChat nom écrire.
sautDeLigne.

". Age du chat : " écrire.
unChat age écrire.
sautDeLigne.

". Race du chat : " écrire.
unChat race écrire.
sautDeLigne.

". Nom du chien : " écrire.
unChien nom écrire.
sautDeLigne.

". Age du chien : " écrire.
unChien age écrire.
sautDeLigne.

". Race du chien : " écrire.
unChien race écrire.
sautDeLigne.
```

**Explication du code**

Ce code démontre les concepts de programmation orientée objet en Smalltalk. Il définit trois classes : `Animal`, `Chat` et `Chien`. La classe `Animal` est la classe parente de `Chat` et `Chien`.

* La classe `Animal` a deux variables d'instance : `nom` et `age`. Elle dispose de méthodes pour obtenir et modifier ces variables.
* La classe `Chat` hérite de la classe `Animal`. Elle ajoute une variable d'instance `race`.
* La classe `Chien` hérite également de la classe `Animal`. Elle ajoute une variable d'instance `race`.

Les classes `Chat` et `Chien` redéfinissent la méthode `nouveau` pour prendre en charge l'initialisation de la variable d'instance `race`.

Le code principal instancie des objets `Chat` et `Chien` et affiche leurs propriétés.

Voici un résumé des méthodes utilisées :

* `nouvelleAvecRace:` : Crée une nouvelle instance de la classe avec une race spécifiée.
* `nouveauAvecNom:age:race:` : Crée une nouvelle instance de la classe avec un nom, un âge et une race spécifiés.
* `nom` : Renvoie le nom de l'animal.
* `changerNom:` : Modifie le nom de l'animal.
* `age` : Renvoie l'âge de l'animal.
* `changerAge:` : Modifie l'âge de l'animal.
* `race` : Renvoie la race de l'animal.
* `changerRace:` : Modifie la race de l'animal.