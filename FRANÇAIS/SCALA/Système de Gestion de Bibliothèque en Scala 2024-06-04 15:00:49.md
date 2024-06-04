**Système de gestion de bibliothèque**

```scala
import scala.io.StdIn._
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

object Bibliotheque {

  // Map pour stocker les livres
  private val livres = Map[Int, Livre]()
  // ListBuffer pour stocker les emprunts
  private val emprunts = ListBuffer[Emprunt]()

  def main(args: Array[String]): Unit = {
    // Boucle principale
    while (true) {
      // Affichage du menu
      println("\nMenu de la bibliothèque:")
      println("1. Ajouter un livre")
      println("2. Emprunter un livre")
      println("3. Retourner un livre")
      println("4. Afficher tous les livres")
      println("5. Afficher tous les emprunts")
      println("0. Quitter")

      // Saisie du choix de l'utilisateur
      val choix = readInt()

      // Traitement du choix
      choix match {
        case 1 => ajouterLivre()
        case 2 => emprunterLivre()
        case 3 => retournerLivre()
        case 4 => afficherLivres()
        case 5 => afficherEmprunts()
        case 0 => System.exit(0)
        case _ => println("Choix invalide")
      }
    }
  }

  // Méthode pour ajouter un livre
  def ajouterLivre(): Unit = {
    // Saisie des informations du livre
    print("Titre du livre: ")
    val titre = readLine()
    print("Nom de l'auteur: ")
    val auteur = readLine()
    print("Numéro d'identification (ID): ")
    val idLivre = readInt()

    // Création du livre et ajout à la map
    val livre = new Livre(idLivre, titre, auteur)
    livres.put(idLivre, livre)

    // Message de confirmation
    println(s"Le livre $titre a bien été ajouté.")
  }

  // Méthode pour emprunter un livre
  def emprunterLivre(): Unit = {
    // Saisie du numéro d'identification du livre
    print("ID du livre à emprunter: ")
    val idLivre = readInt()

    // Recherche du livre dans la map
    val livre = livres.get(idLivre)

    // Si le livre n'existe pas, affichage d'un message
    if (livre.isEmpty) {
      println(s"Le livre avec l'ID $idLivre n'existe pas.")
    }
    // Sinon, emprunt du livre
    else {
      // Saisie des informations de l'emprunteur
      print("Nom de l'emprunteur: ")
      val emprunteur = readLine()
      print("Durée d'emprunt (en jours): ")
      val duree = readInt()

      // Création de l'emprunt et ajout à la liste
      val emprunt = new Emprunt(idLivre, emprunteur, duree)
      emprunts.append(emprunt)

      // Message de confirmation
      println(s"Le livre $livre a bien été emprunté par $emprunteur.")
    }
  }

  // Méthode pour retourner un livre
  def retournerLivre(): Unit = {
    // Saisie du numéro d'identification du livre
    print("ID du livre à retourner: ")
    val idLivre = readInt()

    // Recherche de l'emprunt correspondant dans la liste
    val emprunt = emprunts.find(_.idLivre == idLivre)

    // Si l'emprunt n'existe pas, affichage d'un message
    if (emprunt.isEmpty) {
      println(s"L'emprunt du livre avec l'ID $idLivre n'existe pas.")
    }
    // Sinon, retour du livre
    else {
      emprunts.remove(emprunts.indexOf(emprunt.get))

      // Message de confirmation
      println(s"Le livre $idLivre a bien été retourné.")
    }
  }

  // Méthode pour afficher tous les livres
  def afficherLivres(): Unit = {
    println("Liste des livres:")
    livres.foreach(livre => println(livre._2))
  }

  // Méthode pour afficher tous les emprunts
  def afficherEmprunts(): Unit = {
    println("Liste des emprunts:")
    emprunts.foreach(emprunt => println(emprunt))
  }
}

// Classe représentant un livre
case class Livre(idLivre: Int, titre: String, auteur: String) {
  override def toString: String = s"$titre ($idLivre) par $auteur"
}

// Classe représentant un emprunt
case class Emprunt(idLivre: Int, emprunteur: String, duree: Int) {
  override def toString: String = s"$emprunteur emprunte le livre $idLivre pour $duree jours"
}
```

**Explication du code:**

* Le code implémente un système de gestion de bibliothèque simple en Scala.
* Il utilise des structures de données de la bibliothèque standard de Scala, telles que `Map` et `ListBuffer`, pour stocker les informations sur les livres et les emprunts.
* Le programme s'exécute dans une boucle principale qui affiche un menu à l'utilisateur. L'utilisateur peut choisir d'ajouter un livre, d'emprunter un livre, de retourner un livre, d'afficher tous les livres ou emprunts, ou de quitter le programme.
* Les méthodes `ajouterLivre()`, `emprunterLivre()`, `retournerLivre()`, `afficherLivres()` et `afficherEmprunts()` implémentent les différentes fonctionnalités du système.
* Les classes `Livre` et `Emprunt` représentent des objets livres et emprunts, respectivement.
* Le code utilise la correspondance de modèles pour traiter les choix de l'utilisateur dans le menu principal.
* Le code gère également les cas d'erreur tels que la tentative d'emprunt d'un livre inexistant ou le retour d'un livre qui n'est pas emprunté.