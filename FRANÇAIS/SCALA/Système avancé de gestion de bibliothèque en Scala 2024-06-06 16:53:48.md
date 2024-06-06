**Système de gestion de bibliothèque avancée**

```scala
trait Livre {
  def titre: String
  def auteur: String
  def annee: Int
}

class LivrePapier(override val titre: String, override val auteur: String, override val annee: Int) extends Livre {
  var disponible: Boolean = true
}

class LivreElectronique(override val titre: String, override val auteur: String, override val annee: Int) extends Livre {
  var downloads: Int = 0
}

class Bibliotheque {
  private var livres: List[Livre] = Nil

  def ajouterLivre(livre: Livre): Unit = livres ::= livre

  def rechercherTitre(titreLivre: String): List[Livre] = livres.filter(_.titre == titreLivre)

  def rechercherAuteur(auteurLivre: String): List[Livre] = livres.filter(_.auteur == auteurLivre)

  def rechercherAnnee(anneeLivre: Int): List[Livre] = livres.filter(_.annee == anneeLivre)

  def emprunterLivre(livre: Livre): Unit = livre match {
    case livrePapier: LivrePapier if livrePapier.disponible => livrePapier.disponible = false
    case _ => throw new Exception("Impossible d'emprunter le livre.")
  }

  def rendreLivre(livre: Livre): Unit = livre match {
    case livrePapier: LivrePapier => livrePapier.disponible = true
    case _ => throw new Exception("Impossible de rendre le livre.")
  }

  def afficherLivres(): Unit = {
    println("Livres papier:")
    println("-----------------")
    livres.collect { case livrePapier: LivrePapier => livrePapier }.foreach(println)
    println()
    println("Livres électroniques:")
    println("-------------------------")
    livres.collect { case livreElectronique: LivreElectronique => livreElectronique }.foreach(println)
  }
}
```

**Utilisation du code**

```scala
val bibliotheque = new Bibliotheque()

bibliotheque.ajouterLivre(new LivrePapier("Le Seigneur des anneaux", "J.R.R. Tolkien", 1954))
bibliotheque.ajouterLivre(new LivrePapier("Cent ans de solitude", "Gabriel García Márquez", 1967))
bibliotheque.ajouterLivre(new LivreElectronique("La Servante écarlate", "Margaret Atwood", 1985))
bibliotheque.ajouterLivre(new LivreElectronique("Dune", "Frank Herbert", 1965))

bibliotheque.emprunterLivre(bibliotheque.rechercherTitre("Le Seigneur des anneaux").head)

bibliotheque.afficherLivres()
```

**Explication du code**

* **Interface `Livre`:** Définit les propriétés de base d'un livre.
* **Classes `LivrePapier` et `LivreElectronique`:** Implémentent l'interface `Livre` pour représenter des livres papier et électroniques, respectivement.
* **Classe `Bibliotheque`:** Gère la collection de livres, permet de les ajouter, les rechercher, les emprunter et les rendre.
* **Utilisation:** Crée une instance de bibliothèque, ajoute des livres, emprunte un livre et affiche la liste des livres.