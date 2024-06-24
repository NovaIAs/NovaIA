```scala
// Création d'une classe de base
trait Animal {
  def nom: String
  def age: Int
  def crier(): String
}

// Création de deux sous-classes
class Chat(val nom: String, val age: Int) extends Animal {
  override def crier(): String = "Miaou !"
}

class Chien(val nom: String, val age: Int) extends Animal {
  override def crier(): String = "Ouaf !"
}

// Création d'une liste d'animaux
val animaux = List(new Chat("Félix", 3), new Chien("Médor", 5))

// Affichage du nom et du cri de chaque animal
animaux.foreach(animal => println(s"${animal.nom} : ${animal.crier()}"))

// Création d'une fonction pour comparer les âges des animaux
def comparerAges(a1: Animal, a2: Animal): Int = {
  if (a1.age > a2.age) {
    1
  } else if (a1.age < a2.age) {
    -1
  } else {
    0
  }
}

// Tri de la liste d'animaux par âge croissant
val animauxTries = animaux.sortWith(comparerAges)

// Affichage des animaux triés par âge
animauxTries.foreach(animal => println(s"${animal.nom} : ${animal.age} ans"))
```

**Explication du code :**

Ce code Scala crée deux classes de base, `Animal` et deux sous-classes, `Chat` et `Chien`. La classe `Animal` définit les propriétés `nom` et `age` ainsi qu'une méthode `crier`. Les sous-classes `Chat` et `Chien` redéfinissent la méthode `crier` pour retourner respectivement "Miaou !" et "Ouaf !".

Le code crée ensuite une liste d'objets animaux et affiche le nom et le cri de chaque animal.

Ensuite, une fonction `comparerAges` est définie pour comparer les âges des animaux. Cette fonction est utilisée pour trier la liste d'animaux par âge croissant à l'aide de la méthode `sortWith`.

Enfin, les animaux triés sont affichés.