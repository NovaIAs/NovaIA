**Algorithme de tri rapide en Scheme**

```scheme
(define (quicksort lst)
  (cond
    [(empty? lst) '()]
    [else
     (let ((pivot (car lst))
           (lt (filter (lambda (x) (< x pivot)) (cdr lst)))
           (gt (filter (lambda (x) (> x pivot)) (cdr lst))))
       (cons lt (quicksort gt)))
     ]))
```

**Explication du code :**

* `quicksort` est une procédure qui prend une liste en entrée et renvoie une nouvelle liste triée.
* La procédure utilise l'algorithme de tri rapide, qui fonctionne en choisissant un élément pivot dans la liste et en divisant la liste en deux sous-listes : une contenant les éléments plus petits que le pivot, et l'autre contenant les éléments plus grands que le pivot.
* Les sous-listes sont ensuite triées récursivement.
* La procédure `empty?` vérifie si la liste est vide.
* La procédure `car` renvoie le premier élément de la liste.
* La procédure `cdr` renvoie le reste de la liste, c'est-à-dire tous les éléments sauf le premier.
* La procédure `filter` renvoie une nouvelle liste contenant tous les éléments de la liste d'origine qui satisfont à un prédicat donné.
* La procédure `lt` renvoie la sous-liste contenant les éléments plus petits que le pivot.
* La procédure `gt` renvoie la sous-liste contenant les éléments plus grands que le pivot.
* La procédure `cons` construit une nouvelle liste en ajoutant un élément à sa tête.