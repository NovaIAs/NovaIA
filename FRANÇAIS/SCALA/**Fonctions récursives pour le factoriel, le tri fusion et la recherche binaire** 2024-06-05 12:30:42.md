**Fonction factorielle récursive**

```scala
def factorielle(n: Int): BigInt = {
  if (n <= 0) 1
  else n * factorielle(n - 1)
}
```

Cette fonction utilise la récursivité pour calculer le factoriel d'un entier. Le factoriel d'un nombre est le produit de tous les entiers positifs jusqu'à ce nombre.

**Tri par fusion**

```scala
def triFusion(liste: List[Int]): List[Int] = {

  def fusionner(gauche: List[Int], droite: List[Int]): List[Int] = {
    (gauche, droite) match {
      case (Nil, droite) => droite
      case (gauche, Nil) => gauche
      case (gaucheTete :: gaucheQueue, droiteTete :: droiteQueue) =>
        if (gaucheTete < droiteTete) gaucheTete :: fusionner(gaucheQueue, droite)
        else droiteTete :: fusionner(gauche, droiteQueue)
    }
  }

  if (liste.length <= 1) liste
  else {
    val milieu = liste.length / 2
    val (gauche, droite) = liste.splitAt(milieu)
    fusionner(triFusion(gauche), triFusion(droite))
  }
}
```

Cette fonction tri le contenu d'une liste d'entiers en utilisant l'algorithme de tri par fusion. L'algorithme divise la liste en deux plus petites listes, les trie récursivement, puis fusionne les résultats.

**Recherche binaire**

```scala
def rechercheBinaire(liste: List[Int], cible: Int): Int = {

  def rechercher(gauche: Int, droite: Int): Int = {
    if (gauche > droite) -1
    else {
      val milieu = (gauche + droite) / 2
      val elementMilieu = liste(milieu)
      if (elementMilieu == cible) milieu
      else if (elementMilieu < cible) rechercher(milieu + 1, droite)
      else rechercher(gauche, milieu - 1)
    }
  }

  rechercher(0, liste.length - 1)
}
```

Cette fonction recherche un élément dans une liste triée en utilisant l'algorithme de recherche binaire. L'algorithme divise la liste en deux parties, compare la cible au milieu, puis recherche récursivement dans la partie appropriée.