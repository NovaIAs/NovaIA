**Fonction de recherche de chemin dans un labyrinthe**

```scheme
(define (trouver-chemin maze départ but)
  (define (trouver-chemin-aux maze position chemin-actuel)
    (cond (not (dans-maze? maze position)) #f
          ((= position but) chemin-actuel)
          (else
            (for/fold ((position* (liste-voisins position maze)))
              ((position* (cdr position*))
               (trouver-chemin-aux maze (car position*) (cons (car position*) chemin-actuel))))))

  (if (dans-maze? maze départ)
      (trouver-chemin-aux maze départ '()) #f)
```

**Explication**

Cette fonction utilise la récursion pour trouver un chemin d'un point de départ à un point d'arrivée dans un labyrinthe.

* `trouver-chemin` : La fonction principale qui prend en entrée le labyrinthe, le point de départ et le point d'arrivée. Elle retourne le chemin trouvé ou #f s'il n'y a pas de chemin.

* `trouver-chemin-aux` : Une fonction auxiliaire récursive qui prend en entrée le labyrinthe, une position courante et le chemin actuel. Elle explore les voisins de la position courante et retourne le chemin s'il existe un chemin vers l'arrivée.

* `dans-maze?` : Une fonction auxiliaire qui vérifie si une position est à l'intérieur du labyrinthe.

* `liste-voisins` : Une fonction auxiliaire qui retourne une liste des voisins d'une position dans le labyrinthe.

* `for/fold` : Une macro qui effectue un parcours itératif sur une liste, en accumulant un résultat à chaque étape. Dans ce cas, elle explore les voisins de la position courante et retourne le chemin si un chemin est trouvé.