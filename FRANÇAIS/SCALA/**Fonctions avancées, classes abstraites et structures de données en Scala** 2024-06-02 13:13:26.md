```scala
// Fonctions de haut niveau
val somme = (x: Int, y: Int) => x + y // Fonction lambda pour sommer deux nombres
val filtrePair = (liste: List[Int]) => liste.filter(_ % 2 == 0) // Fonction pour filtrer les nombres pairs d'une liste

// Classes abstraites et traits
trait Animal {
  def crier(): String // Méthode abstraite pour obtenir le cri de l'animal
}

class Chien extends Animal {
  override def crier(): String = "Ouaf !" // Implémentation de crier() pour la classe Chien
}

class Chat extends Animal {
  override def crier(): String = "Miaou !" // Implémentation de crier() pour la classe Chat
}

// Structures de données
val listeNombres: List[Int] = List(1, 2, 3, 4, 5) // Liste d'entiers
val carteVilles: Map[String, String] = Map("Paris" -> "France", "Londres" -> "Angleterre") // Carte associant des villes à des pays

// Utilisation de fonctions et structures de données
val sommeNombresPairs = listeNombres.filter(filtrePair).map(somme) // Appliquer les fonctions filtrePair et somme à la liste de nombres

// Patterns matching
def quelJour(jour: String): String = {
  jour match {
    case "lundi" => "Début de la semaine"
    case "mardi" | "mercredi" | "jeudi" => "Milieu de la semaine"
    case "vendredi" => "Fin de la semaine"
    case _ => "Jour inconnu"
  }
}

// Manipulation de fichiers
import java.io.File
val fichier = new File("monFichier.txt")
val texteFichier = scala.io.Source.fromFile(fichier).mkString // Lire le contenu du fichier dans une chaîne

// Concurrence
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
val futureSomme = Future { somme(10, 20) } // Créer une tâche future pour sommer deux nombres
futureSomme.onComplete { case result => println(s"Résultat de la somme : ${result.get}") } // Traiter le résultat de la tâche future

```