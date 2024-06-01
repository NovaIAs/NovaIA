**Module de tri avancé en Scheme**

```scheme
(define (tri-rapide xs)
  (cond ((vide? xs) '())
        ((= 1 (longueur xs)) xs)
        (else
          (let ((pivot (car xs)))
            (append (tri-rapide (filtre (lambda (x) (< x pivot)) (cdr xs)))
                    (cons pivot)
                    (tri-rapide (filtre (lambda (x) (>= x pivot)) (cdr xs)))))))
```

**Explication du code :**

Ce module implémente l'algorithme de tri rapide en Scheme. Il triera une liste de nombres dans l'ordre croissant.

* **tri-rapide** est la fonction principale de tri. Elle prend une liste `xs` comme paramètre.
* `vide?` vérifie si la liste est vide.
* `longueur` renvoie la longueur de la liste.
* `car` retourne le premier élément de la liste.
* `cdr` renvoie le reste de la liste après le premier élément.
* `filtre` renvoie une nouvelle liste contenant uniquement les éléments qui satisfont au prédicat donné.
* `cons` construit une nouvelle liste en préfixant un élément à la liste existante.

**Fonctionnement :**

L'algorithme de tri rapide fonctionne en choisissant un élément pivot dans la liste. Il partitionne ensuite la liste en deux sous-listes : une contenant les éléments inférieurs au pivot et l'autre contenant les éléments supérieurs ou égaux au pivot. L'algorithme est ensuite appliqué récursivement à chaque sous-liste, puis les sous-listes triées sont concaténées pour former la liste triée finale.