**Programme de calcul d'impôt sur le revenu (avec tranches d'imposition)**

```scala
object ImpôtRevenu {

  // Définition des tranches d'imposition
  val tranches = Map(
    0.0 -> 0.0,
    10000.0 -> 0.1,
    20000.0 -> 0.15,
    30000.0 -> 0.2,
    40000.0 -> 0.25,
    50000.0 -> 0.3
  )

  // Fonction de calcul de l'impôt
  def calculImpôt(revenu: Double): Double = {
    var impôt = 0.0
    var revenuRestant = revenu

    // Boucle sur les tranches d'imposition
    for ((borne, taux) <- tranches) {
      // Si le revenu restant est supérieur à la borne
      if (revenuRestant > borne) {
        // Calcul de l'impôt sur la tranche
        impôt += (Math.min(revenuRestant, borne + 1) - borne) * taux

        // Mise à jour du revenu restant
        revenuRestant -= (Math.min(revenuRestant, borne + 1) - borne)
      }
    }

    // Retour de l'impôt calculé
    impôt
  }

  def main(args: Array[String]): Unit = {
    // Demande du revenu à l'utilisateur
    println("Entrez votre revenu imposable : ")
    val revenu = scala.io.StdIn.readLine().toDouble

    // Calcul de l'impôt
    val impôt = calculImpôt(revenu)

    // Affichage du résultat
    println(s"Votre impôt sur le revenu est de : $impôt €")
  }
}
```

**Explications du code :**

* La map `tranches` définit les différentes tranches d'imposition, avec le taux d'imposition associé à chaque tranche.
* La fonction `calculImpôt` calcule l'impôt à payer en fonction du revenu fourni en argument.
* Pour chaque tranche d'imposition, elle calcule l'impôt sur la partie du revenu qui se trouve dans cette tranche (`Math.min(revenuRestant, borne + 1) - borne`).
* L'impôt total est ensuite retourné.
* La fonction `main` demande le revenu à l'utilisateur, calcule l'impôt en appelant `calculImpôt`, puis affiche le résultat.