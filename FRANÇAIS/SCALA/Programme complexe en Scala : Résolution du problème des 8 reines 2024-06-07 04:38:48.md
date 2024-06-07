**Programme complexe en Scala**

**But :** Résoudre le problème des 8 reines

**Code :**

```scala
object HuitReines extends App {

  // Classe représentant une position sur l'échiquier
  class Position(val ligne: Int, val colonne: Int) {
    override def toString: String = s"($ligne, $colonne)"
  }

  // Classe représentant une solution au problème des 8 reines
  class Solution(val positions: List[Position]) {
    override def toString: String = positions.mkString(", ")
  }

  // Fonction récursive pour générer toutes les solutions possibles
  def solutions(tailleEchiquier: Int, positions: List[Position]): List[Solution] = {
    if (positions.size == tailleEchiquier) List(Solution(positions))
    else {
      (0 until tailleEchiquier).toList.flatMap { colonne =>
        if (!positions.exists(p => p.ligne == positions.head.ligne + colonne - positions.size || p.colonne == colonne))
          solutions(tailleEchiquier, positions :+ new Position(positions.size, colonne))
        else Nil
      }
    }
  }

  // Résolution du problème et affichage de la première solution trouvée
  val solution = solutions(8, Nil).head
  println(solution)
}
```

**Explication :**

* **Classe Position :** Reprète une position sur un échiquier par une ligne et une colonne.
* **Classe Solution :** Reprète une solution au problème des 8 reines sous la forme d'une liste de positions.
* **Fonction solutions :** Génère récursivement toutes les solutions possibles au problème. Elle prend en paramètre la taille de l'échiquier et une liste de positions déjà occupées par des reines.
* Le programme résout le problème et affiche la première solution trouvée.

**Complexité :**

* Le nombre de solutions pour un échiquier de taille n est d'environ n^n.
* La fonction solutions explore toutes les possibilités, ce qui entraîne une complexité exponentielle (O(n^n)).