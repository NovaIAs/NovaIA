**Algorithme génétique pour optimiser une fonction**

```scheme
(define (genetic-algorithm f n-generations n-individus mutation-rate crossover-rate)
  ;; Crée une population initiale
  (define population (make-list (random-real n-individus) n-générations))
  ;; Initialise la meilleure solution
  (define (best-solution n) (car (sort ((lambda (a b) (> (f b) (f a))) n))))

  ;; Boucle principale de l'algorithme génétique
  (for/loop ((g 0 (+ g 1) (< g n-générations)))
    ;; Évalue la population
    (for/loop ((i 0 (+ i 1) (< i n-individus)))
      (define fitness (f (list-ref population g i))))

    ;; Sélectionne les individus pour la reproduction
    (define selected-population (select-individuals population fitness n-individus))

    ;; Reproduit les individus
    (define new-population (reproduce selected-population mutation-rate crossover-rate))

    ;; Remplace la population actuelle par la nouvelle population
    (set! population new-population))

  ;; Renvoie la meilleure solution
  (best-solution population))

;; Crée une population initiale
(define (make-list init n) (do ((i 0 (+ i 1) (< i n)) (result '())) ((= i n) result) (cons init result)))

;; Trie une liste selon une fonction de comparaison
(define (sort cmp n) (foldl (lambda (x y) (insert-sorted cmp x y)) n null))

;; Insère un élément dans une liste triée selon une fonction de comparaison
(define (insert-sorted cmp x n)
  (cond ((null? n) (list x))
        ((cmp x (car n)) (cons x n))
        (else (cons (car n) (insert-sorted cmp x (cdr n))))))

;; Sélectionne les individus pour la reproduction
(define (select-individuals population fitness n)
  (foldl (lambda (i o) (cons (list-ref population i) o))
      (sort (lambda (a b) (<= (cadar a) (cadar b)))
          (map (lambda (i) (cons i (car (list-ref population i))))
              (for/list n)))
      null))

;; Reproduit les individus
(define (reproduce population mutation-rate crossover-rate)
  (map (lambda (i)
    (reproduce-individual (list-ref population i) (list-ref population (random n-individus))
                mutation-rate crossover-rate))
      (for/list n-individus)))

;; Reproduit un individu
(define (reproduce-individual father mother mutation-rate crossover-rate)
  (map (lambda (i) (reproduce-gene (list-ref father i) (list-ref mother i) mutation-rate crossover-rate))
      (for/list n-gens)))
```

**Explication du code**

Cet algorithme génétique optimise une fonction en utilisant des techniques d'évolution. Il crée une population initiale de solutions aléatoires, évalue chaque solution, sélectionne les meilleures solutions pour la reproduction, reproduit les solutions et remplace la population actuelle par la nouvelle population. Ce processus est répété pendant un nombre prédéterminé de générations. La meilleure solution est sélectionnée dans la population finale.

* **`genetic-algorithm`** : fonction principale qui implémente l'algorithme génétique.
* **`make-list`** : crée une population initiale de solutions aléatoires.
* **`sort`** : trie une liste selon une fonction de comparaison.
* **`insert-sorted`** : insère un élément dans une liste triée.
* **`select-individuals`** : sélectionne les individus pour la reproduction en fonction de leur fitness.
* **`reproduce`** : reproduit les individus en utilisant la mutation et le croisement.
* **`reproduce-individual`** : reproduit un individu en utilisant la mutation et le croisement.
* **`reproduce-gene`** : reproduit un gène en utilisant la mutation et le croisement.