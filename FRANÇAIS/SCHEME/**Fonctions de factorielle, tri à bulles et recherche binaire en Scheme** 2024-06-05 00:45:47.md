**Programme Factorielle en Scheme**

```scheme
(define (factorielle n)
  (if (eq? n 0)
    1
    (* n (factorielle (- n 1)))))
```

**Explication:**

Cette fonction calcule la factorielle d'un nombre entier `n`. La factorielle d'un nombre est le produit de tous les entiers positifs inférieurs ou égaux à `n`.

La fonction utilise la récursivité pour calculer la factorielle. Si `n` est égal à 0, la fonction renvoie 1 car la factorielle de 0 est définie comme 1. Sinon, la fonction multiplie `n` par la factorielle de `n-1`.

**Programme de Tri à Bulles en Scheme**

```scheme
(define (tri-a-bulles lst)
  (define (tri-interne lst)
    (if (empty? lst)
      lst
      (let loop ((acc lst) (reste (cdr lst)))
        (if (empty? reste)
          (cons (car lst) acc)
          (loop (cons (mini (car lst) (car reste)) acc) (cons (maxi (car lst) (car reste)) reste))))))
  (tri-interne (reverse lst)))
```

**Explication:**

Cette fonction trie une liste `lst` en utilisant l'algorithme de tri à bulles. Le tri à bulles compare chaque paire d'éléments adjacents dans la liste et les échange si nécessaire.

La fonction utilise une fonction interne `tri-interne` qui prend une liste et la trie récursivement. La fonction interne vérifie si la liste est vide et, si c'est le cas, renvoie la liste inchangée. Si ce n'est pas le cas, la fonction compare les premier et deuxième éléments de la liste à l'aide des fonctions `mini` et `maxi`.

La fonction `mini` renvoie le plus petit des deux éléments et la fonction `maxi` renvoie le plus grand. La fonction interne utilise ensuite une boucle pour comparer les éléments restants de la liste et les insérer dans la liste triée `acc`.

La fonction externe `tri-a-bulles` inverse la liste d'entrée avant de l'appeler sur la fonction interne `tri-interne` afin de trier la liste dans l'ordre croissant.

**Programme de Recherche Binaire en Scheme**

```scheme
(define (recherche-binaire lst x)
  (define (recherche-interne lst x lo hi)
    (cond ((< lo hi) #f)
          ((= lo hi) (eq? x (car lst)))
          (else
            (let ((mi (+ lo hi (/ (- hi lo) 2))))
              (cond ((< x (car lst))
                    (recherche-interne (cdr lst) x lo mi))
                    ((> x (car lst))
                    (recherche-interne (cdr lst) x (1+ mi) hi))
                    (else
                      #t)))))))
  (recherche-interne lst x 0 (length lst)))
```

**Explication:**

Cette fonction effectue une recherche binaire dans une liste `lst` pour un élément `x`. La recherche binaire est un algorithme de recherche efficace qui divise de manière récursive la liste en deux moitiés jusqu'à ce que l'élément soit trouvé.

La fonction utilise une fonction interne `recherche-interne` qui prend une liste, un élément à rechercher, un index de début `lo` et un index de fin `hi`.

La fonction interne vérifie d'abord si `lo` est supérieur ou égal à `hi`, auquel cas elle renvoie `#f` pour indiquer que l'élément n'a pas été trouvé. Si `lo` est égal à `hi`, elle vérifie si l'élément à rechercher est égal au premier élément de la liste et renvoie `#t` si c'est le cas.

Dans le cas contraire, la fonction interne calcule un index intermédiaire `mi` et compare l'élément à rechercher au `mi`-ème élément de la liste. Si l'élément à rechercher est inférieur au `mi`-ème élément, la fonction interne appelle elle-même de manière récursive sur la première moitié de la liste. Si l'élément à rechercher est supérieur au `mi`-ème élément, la fonction interne appelle elle-même de manière récursive sur la seconde moitié de la liste. Si l'élément à rechercher est égal au `mi`-ème élément, la fonction interne renvoie `#t` pour indiquer que l'élément a été trouvé.

La fonction externe `recherche-binaire` appelle la fonction interne avec un indice de début de 0 et un indice de fin égal à la longueur de la liste.