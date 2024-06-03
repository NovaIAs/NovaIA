```scheme
(begin
  (define jalon1 (lambda (l n i)
                  (if (= i n)
                      l
                      (cons (cons (car l) '())
                            (jalon1 (cdr l) n (1+ i))))))

  (define minmax (lambda (l)
                   (if (null? l)
                       '#()
                       (let ((mi (car l))
                             (ma (car l)))
                         (for ((x l))
                           (if (< x mi)
                               (set! mi x)
                               ())
                           (if (> x ma)
                               (set! ma x)
                               ()))
                         (cons mi ma)))))

  (define divise (lambda (n i)
                   (if (= 0 (modulo n i))
                       '#t
                       '#f)))

  (define triang (lambda (n)
                   (if (< n 0)
                       0
                       (1+ (triang (- n 1))))))

  (define recherche (lambda (l n)
                    (if (empty? l)
                        '#f
                        (if (= (car l) n)
                            '#t
                            (recherche (cdr l) n))))

  (define compteur (lambda (l n)
                   (if (empty? l)
                       0
                       (if (= (car l) n)
                           (1+ (compteur (cdr l) n))
                           (compteur (cdr l) n)))))

  (define filter (lambda (l n)
                   (if (empty? l)
                       '()
                       (if (= (car l) n)
                           (cons n (filter (cdr l) n))
                           (filter (cdr l) n)))))

  (define compress (lambda (l)
                   (if (empty? l)
                       '()
                       (cons (car l) (compress (filter (cdr l) (car l))))))

  (define somme (lambda (l)
                   (if (empty? l)
                       0
                       (1+ (somme (cdr l))))))

  (define produit (lambda (l)
                   (if (empty? l)
                       1
                       (* (car l) (produit (cdr l))))))

  (define moyenne (lambda (l)
                   (if (empty? l)
                       0.0
                       (/ (somme l) (somme (cons - 1 l)))))
)
```

**Ce code fournit les fonctions suivantes :**

* `jalon1 :` crée un jalon dans une liste à la position spécifiée
* `minmax :` retourne le minimum et le maximum d'une liste
* `divise :` vérifie si un nombre est divisible par un autre
* `triang :` calcule le n-ième nombre triangulaire
* `recherche :` recherche un élément dans une liste
* `compteur :` compte les occurrences d'un élément dans une liste
* `filter :` supprime les éléments d'une liste qui ne correspondent pas à une valeur donnée
* `compress :` supprime les éléments en double dans une liste
* `somme :` calcule la somme des éléments d'une liste
* `produit :` calcule le produit des éléments d'une liste
* `moyenne :` calcule la moyenne des éléments d'une liste