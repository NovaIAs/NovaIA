**Code complexe en Scala**

```scala
// Définition de classes et de traits

trait Animal {
  def nom: String
  def crier(): Unit
}

class Chat extends Animal {
  override def nom: String = "Chat"
  override def crier(): Unit = println("Miaou !")
}

class Chien extends Animal {
  override def nom: String = "Chien"
  override def crier(): Unit = println("Wouaf !")
}

// Définition d'objets compagnons

object Animal {
  def creerAnimal(typeAnimal: String): Option[Animal] = {
    typeAnimal match {
      case "Chat" => Some(new Chat)
      case "Chien" => Some(new Chien)
      case _ => None
    }
  }
}

// Définition d'une fonction générique

def afficherAnimaux[T <: Animal](animaux: Seq[T]): Unit = {
  animaux.foreach(_.crier())
}

// Définition d'une classe générique

class Zoo[T <: Animal] {
  private val animaux: List[T] = Nil

  def ajouterAnimaux(nouveauxAnimaux: Seq[T]): Unit = {
    animaux :::= nouveauxAnimaux
  }

  def afficherAnimaux(): Unit = {
    animaux.foreach(_.crier())
  }
}

// Utilisation du code

val chat = Animal.creerAnimal("Chat")
val chien = Animal.creerAnimal("Chien")

if (chat.isDefined) {
  afficherAnimaux(Seq(chat.get, chien.get))
}

val zoo = new Zoo[Animal]
zoo.ajouterAnimaux(Seq(chat.get, chien.get))
zoo.afficherAnimaux()
```

**Explication du code**

Ce code illustre plusieurs concepts complexes en Scala, notamment :

* **Classes et traits :** Les traits définissent des interfaces communes, tandis que les classes en héritent pour implémenter ces interfaces.
* **Objets compagnons :** Les objets compagnons fournissent une logique statique pour leurs classes associées.
* **Fonctions génériques :** Les fonctions génériques peuvent être utilisées avec différents types de paramètres.
* **Classes génériques :** Les classes génériques peuvent stocker des instances de différents types.
* **Utilisation d'options :** Les options représentent des valeurs pouvant être nulles.
* **Appel par nom :** L'appel par nom retarde l'évaluation des arguments de fonction jusqu'à ce qu'ils soient nécessaires.

Ce code permet de créer et de gérer un zoo d'animaux. Il démontre l'utilisation de classes, de traits, de fonctions génériques, de classes génériques et d'options.