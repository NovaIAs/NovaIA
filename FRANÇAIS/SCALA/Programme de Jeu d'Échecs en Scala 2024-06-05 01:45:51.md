**Programme de jeu d'échecs en Scala**

```scala
import scala.util.Random

class Échiquier {
  private val pièces = Array.fill(64)(0)

  def déplacement(origine: Int, destination: Int): Boolean = {
    val pièce = pièces(origine)
    (pièce & 0xF) match {
      case Pion => déplacementPion(pièce, origine, destination)
      case Tour => déplacementTour(pièce, origine, destination)
      case Cavalier => déplacementCavalier(pièce, origine, destination)
      case Fou => déplacementFou(pièce, origine, destination)
      case Reine => déplacementReine(pièce, origine, destination)
      case Roi => déplacementRoi(pièce, origine, destination)
      case _ => false
    }
  }

  private def déplacementPion(pièce: Int, origine: Int, destination: Int): Boolean = {
    val couleur = pièce >> 4
    val delta = if (couleur == 0) -1 else 1
    (origine + delta * 16 == destination) ||
      (origine + delta * 16 * 2 == destination && origine % 16 == 1)
  }

  private def déplacementTour(pièce: Int, origine: Int, destination: Int): Boolean = {
    val ligne = origine / 16
    val colonne = origine % 16
    (ligne == destination / 16 || colonne == destination % 16)
  }

  private def déplacementCavalier(pièce: Int, origine: Int, destination: Int): Boolean = {
    val deltaLignes = Math.abs(origine / 16 - destination / 16)
    val deltaColonnes = Math.abs(origine % 16 - destination % 16)
    (deltaLignes == 1 && deltaColonnes == 2) || (deltaLignes == 2 && deltaColonnes == 1)
  }

  private def déplacementFou(pièce: Int, origine: Int, destination: Int): Boolean = {
    val deltaLignes = Math.abs(origine / 16 - destination / 16)
    val deltaColonnes = Math.abs(origine % 16 - destination % 16)
    deltaLignes == deltaColonnes
  }

  private def déplacementReine(pièce: Int, origine: Int, destination: Int): Boolean = {
    déplacementTour(pièce, origine, destination) || déplacementFou(pièce, origine, destination)
  }

  private def déplacementRoi(pièce: Int, origine: Int, destination: Int): Boolean = {
    val deltaLignes = Math.abs(origine / 16 - destination / 16)
    val deltaColonnes = Math.abs(origine % 16 - destination % 16)
    (deltaLignes <= 1 && deltaColonnes <= 1)
  }
}

object JeuÉchecs {
  private val échiquier = new Échiquier

  def main(args: Array[String]): Unit = {
    var enCours = true
    var joueur = 0

    while (enCours) {
      val (origine, destination) = obtenirMouvement(joueur)
      if (échiquier.déplacement(origine, destination)) {
        // Mettre à jour l'échiquier
        joueur = 1 - joueur
      } else {
        // Mouvement invalide
        println("Mouvement invalide !")
      }
      // Vérifier si la partie est terminée
      enCours = !partieTerminée(joueur)
    }

    // Annoncer le vainqueur
    println("Le joueur " + (1 - joueur) + " a gagné !")
  }

  private def obtenirMouvement(joueur: Int): (Int, Int) = {
    println("Joueur " + joueur + ", entrez votre mouvement (origine, destination) :")
    val mouvement = Random.nextInt(64) * 2
    (mouvement / 2, mouvement % 2)
  }

  private def partieTerminée(joueur: Int): Boolean = {
    // Vérifier si le roi du joueur est en échec et mat
    false
  }
}
```

**Explications du code :**

* La classe `Échiquier` représente l'échiquier et gère les déplacements des pièces.
* L'objet `JeuÉchecs` est le point d'entrée du jeu et gère le déroulement de la partie.
* Les constantes d'énumération `Pièce` représentent les différents types de pièces d'échecs.

**Déplacement des pièces :**

Chaque méthode de déplacement dans la classe `Échiquier` vérifie les règles de déplacement spécifiques à chaque type de pièce. Les coordonnées `origine` et `destination` sont des indices de tableau représentant les cases sur l'échiquier.

**Gestion de la partie :**

L'objet `JeuÉchecs` gère le déroulement de la partie, y compris la saisie des mouvements des joueurs et la vérification de la fin de la partie.

**Saisie des mouvements :**

La méthode `obtenirMouvement` génère un mouvement aléatoire pour chaque joueur à des fins de démonstration. En réalité, les mouvements seraient saisis par les joueurs.

**Fin de la partie :**

La méthode `partieTerminée` vérifie si le roi du joueur actuel est en échec et mat. Cette vérification n'est pas implémentée dans cet exemple de code, mais elle serait essentielle dans un jeu réel.