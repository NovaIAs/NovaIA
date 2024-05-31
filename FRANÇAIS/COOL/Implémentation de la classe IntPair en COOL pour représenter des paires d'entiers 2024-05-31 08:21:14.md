```cool
creation classe IntPair {
  entier x, y ;

  debut creation IntPair(x : entier, y : entier) {
    x := x ;
    y := y ;
  } fin creation

  fonction get_x() : entier {
    renvoyer x ;
  }

  fonction get_y() : entier {
    renvoyer y ;
  }

  fonction somme(autre : IntPair) : IntPair {
    renvoyer creation IntPair(x + autre.get_x(), y + autre.get_y()) ;
  }

  fonction produit(autre : IntPair) : entier {
    renvoyer x * autre.get_x() + y * autre.get_y() ;
  }

  fonction est_nul() : bool {
    renvoyer x = 0 et y = 0 ;
  }

  fonction est_egal(autre : IntPair) : bool {
    renvoyer est_nul() et autre.est_nul() ou (x = autre.get_x() et y = autre.get_y()) ;
  }

  methode afficher() {
    ecrire("(", x, ", ", y, ")") ;
  }
} fin classe

fonction main() {
  variable paire1, paire2 : IntPair ;
  paire1 := creation IntPair(1, 2) ;
  paire2 := creation IntPair(3, 4) ;

  ecrire("Paire 1 : ") ;
  paire1.afficher() ;
  ecrireln() ;
  ecrire("Paire 2 : ") ;
  paire2.afficher() ;
  ecrireln() ;

  variable somme : IntPair ;
  somme := paire1.somme(paire2) ;
  ecrire("Paire somme : ") ;
  somme.afficher() ;
  ecrireln() ;

  variable produit : entier ;
  produit := paire1.produit(paire2) ;
  ecrire("Produit des paires : ", produit) ;
  ecrireln() ;

  variable est_nulle : bool ;
  est_nulle := paire1.est_nul() ;
  ecrire("Est-elle nulle ? ") ;
  ecrireln(est_nulle) ;

  variable sont_egales : bool ;
  sont_egales := paire1.est_egal(paire2) ;
  ecrire("Sont-elles égales ? ") ;
  ecrireln(sont_egales) ;
}
```

**Explication du code**

Ce code crée une classe `IntPair` en COOL, qui représente une paire d'entiers. La classe possède deux champs, `x` et `y`, qui stockent les valeurs des entiers. Elle fournit également des méthodes pour obtenir les valeurs des champs, calculer la somme et le produit de deux paires d'entiers, vérifier si une paire est nulle et comparer deux paires pour l'égalité.

La fonction `main` crée deux instances de la classe `IntPair` et affiche leurs valeurs. Elle calcule ensuite la somme et le produit des deux paires et les affiche. Elle vérifie également si la première paire est nulle et si les deux paires sont égales. Les résultats sont affichés sur la console.

Ce code illustre les concepts de classes, d'objets, de méthodes et de gestion de données en COOL. Il met en œuvre un exemple complexe qui montre comment utiliser les différents éléments du langage pour créer des programmes plus complexes.