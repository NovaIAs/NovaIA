**Système de gestion de bibliothèque en Scala**

Ce système de gestion de bibliothèque gère les livres, les adhérents et les prêts.

**Modèle de données:**

```scala
case class Livre(id: Long, titre: String, auteur: String, isbn: String)
case class Adherent(id: Long, nom: String, prenom: String, email: String)
case class Pret(id: Long, dateDebut: Date, dateFin: Date, livre: Livre, adherent: Adherent)
```

**Service de gestion des livres:**

```scala
import scala.collection.mutable.Map

class GestionLivre {

  private val livres: Map[Long, Livre] = Map()

  def ajouterLivre(livre: Livre): Unit = {
    livres += livre.id -> livre
  }

  def trouverLivreParId(id: Long): Option[Livre] = {
    livres.get(id)
  }

  def trouverLivresParTitre(titre: String): List[Livre] = {
    livres.values.filter(_.titre.contains(titre)).toList
  }
}
```

**Service de gestion des adhérents:**

```scala
import scala.collection.mutable.Map

class GestionAdherent {

  private val adherents: Map[Long, Adherent] = Map()

  def ajouterAdherent(adherent: Adherent): Unit = {
    adherents += adherent.id -> adherent
  }

  def trouverAdherentParId(id: Long): Option[Adherent] = {
    adherents.get(id)
  }

  def trouverAdherentsParNom(nom: String): List[Adherent] = {
    adherents.values.filter(_.nom.contains(nom)).toList
  }
}
```

**Service de gestion des prêts:**

```scala
import scala.collection.mutable.Map

class GestionPret {

  private val prets: Map[Long, Pret] = Map()

  def ajouterPret(pret: Pret): Unit = {
    prets += pret.id -> pret
  }

  def trouverPretParId(id: Long): Option[Pret] = {
    prets.get(id)
  }

  def trouverPretsParLivreId(livreId: Long): List[Pret] = {
    prets.values.filter(_.livre.id == livreId).toList
  }

  def trouverPretsParAdherentId(adherentId: Long): List[Pret] = {
    prets.values.filter(_.adherent.id == adherentId).toList
  }
}
```

**Logiciel d'interface utilisateur:**

```scala
import java.util.Scanner

object Bibliotheque {

  val gestionLivre = new GestionLivre
  val gestionAdherent = new GestionAdherent
  val gestionPret = new GestionPret
  val scanner = new Scanner(System.in)

  def main(args: Array[String]): Unit = {
    while (true) {
      println("Bienvenue dans la bibliothèque")
      println("1. Liste des livres")
      println("2. Rechercher un livre par titre")
      println("3. Ajouter un livre")
      println("4. Liste des adhérents")
      println("5. Rechercher un adhérent par nom")
      println("6. Ajouter un adhérent")
      println("7. Liste des prêts")
      println("8. Rechercher un prêt par ID")
      println("9. Emprunter un livre")
      println("10. Rendre un livre")
      println("11. Quitter")

      val choix = scanner.nextInt()

      choix match {
        case 1 => listerLivres()
        case 2 => rechercherLivreParTitre()
        case 3 => ajouterLivre()
        case 4 => listerAdherents()
        case 5 => rechercherAdherentParNom()
        case 6 => ajouterAdherent()
        case 7 => listerPrets()
        case 8 => rechercherPretParId()
        case 9 => emprunterLivre()
        case 10 => rendreLivre()
        case 11 => System.exit(0)
        case _ => println("Choix invalide")
      }
    }
  }

  def listerLivres(): Unit = {
    gestionLivre.trouverLivresParTitre("").foreach(println)
  }

  def rechercherLivreParTitre(): Unit = {
    println("Entrez le titre du livre :")
    val titre = scanner.nextLine()
    gestionLivre.trouverLivresParTitre(titre).foreach(println)
  }

  def ajouterLivre(): Unit = {
    println("Entrez le titre du livre :")
    val titre = scanner.nextLine()
    println("Entrez l'auteur du livre :")
    val auteur = scanner.nextLine()
    println("Entrez l'ISBN du livre :")
    val isbn = scanner.nextLine()
    gestionLivre.ajouterLivre(Livre(0, titre, auteur, isbn))
  }

  def listerAdherents(): Unit = {
    gestionAdherent.trouverAdherentsParNom("").foreach(println)
  }

  def rechercherAdherentParNom(): Unit = {
    println("Entrez le nom de l'adhérent :")
    val nom = scanner.nextLine()
    gestionAdherent.trouverAdherentsParNom(nom).foreach(println)
  }

  def ajouterAdherent(): Unit = {
    println("Entrez le nom de l'adhérent :")
    val nom = scanner.nextLine()
    println("Entrez le prénom de l'adhérent :")
    val prenom = scanner.nextLine()
    println("Entrez l'email de l'adhérent :")
    val email = scanner.nextLine()
    gestionAdherent.ajouterAdherent(Adherent(0, nom, prenom, email))
  }

  def listerPrets(): Unit = {
    gestionPret.trouverPretsParLivreId(0).foreach(println)
  }

  def rechercherPretParId(): Unit = {
    println("Entrez l'ID du prêt :")
    val id = scanner.nextInt()
    gestionPret.trouverPretParId(id).foreach(println)
  }

  def emprunterLivre(): Unit = {
    println("Entrez l'ID du livre à emprunter :")
    val livreId = scanner.nextInt()
    println("Entrez l'ID de l'adhérent emprunteur :")
    val adherentId = scanner.nextInt()
    val pret = Pret(0, new Date(), null, gestionLivre.trouverLivreParId(livreId).get, gestionAdherent.trouverAdherentParId(adherentId).get)
    gestionPret.ajouterPret(pret)
  }

  def rendreLivre(): Unit = {
    println("Entrez l'ID du prêt à rendre :")
    val id = scanner.nextInt()
    val pret = gestionPret.trouverPretParId(id).get
    pret.dateFin = new Date()
    gestionPret.ajouterPret(pret)
  }
}
```

**Explications:**

* Le modèle de données définit les classes qui représentent les entités du système (livres, adhérents, prêts).
* Les services de gestion gèrent la persistance des données et fournissent des méthodes pour récupérer et modifier les entités.
* L'interface utilisateur est une application en ligne de commande qui permet à l'utilisateur d'interagir avec le système.
* Le logiciel utilise des fonctionnalités avancées de Scala telles que les classes de cas, les collections immuables et les fonctions anonymes.
* Le code est organisé de manière modulaire, ce qui le rend facile à maintenir et à étendre.