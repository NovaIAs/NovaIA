**Programme Complexe en Scheme**

**Objectif:** Générer une grille de Sudoku partiellement résolue, avec un nombre spécifique d'indices donnés.

**Code:**

```scheme
;;; Importer les bibliothèques nécessaires
(require racket/random racket/list)

;;; Exemple de grille de Sudoku vide
(define empty-grid (make-vector 9 (make-vector 9 0)))

;;; Fonction pour générer une liste de nombres aléatoires sans répétition
(define (random-list n)
  (random-sample n (list->vector (numbers-within n))))

;;; Fonction pour initialiser la grille avec une ligne remplie
(define (init-row grid row-idx values)
  (map (lambda (col-idx)
         (set-vector! grid row-idx col-idx (list-ref values col-idx)))
       (numbers-within 9)))

;;; Fonction pour placer un nombre aléatoire dans une case vide
(define (fill-empty-cell grid)
  (define empty-cells
    (filter (lambda (cell) (zero? (vector-ref grid cell)))
            (cartesian-product (numbers-within 9) (numbers-within 9))))
  (define (pick-cell) (random-sample 1 empty-cells))
  (define (set-number cell)
    (set-vector! grid (first cell) (second cell)
                 (random-sample 1 (numbers-within 9))))
  (lazy-eval (andmap set-number (random-list 1 empty-cells))))

;;; Fonction pour générer une grille de Sudoku partiellement résolue
(define (generate-puzzle num-clues)
  (define grid empty-grid)
  (define clues (random-list num-clues))
  (map (lambda (clue)
         (define row (vector-ref clues clue)
                   col (vector-ref clues clue 2)
                   value (vector-ref clues clue 3))
         (init-row grid row value)))
  (lazy-eval (repeat-lazy (lambda ()
                              (fill-empty-cell grid))
                            (- 81 num-clues))))

;;; Fonction pour afficher une grille de Sudoku
(define (display-grid grid)
  (map (lambda (row)
         (printf "~a\n" (vector->list row)))
       (transpose grid)))
```

**Explication du Code:**

* **Bibliothèques:** Le code utilise les bibliothèques `racket/random` et `racket/list` pour la génération de nombres aléatoires et la manipulation de listes.
* **Grille vide:** Une grille de Sudoku vide est représentée par une matrice 9x9 de zéros.
* **Génération de nombres aléatoires:** La fonction `random-list` génère une liste de nombres aléatoires sans répétition.
* **Initialisation de ligne:** La fonction `init-row` initialise une ligne de la grille avec une liste de nombres.
* **Placement de nombres aléatoires:** La fonction `fill-empty-cell` place un nombre aléatoire dans une case vide de la grille.
* **Génération de grille partiellement résolue:** La fonction `generate-puzzle` génère une grille de Sudoku partiellement résolue en initialisant une grille vide, en ajoutant des indices aléatoires et en plaçant des nombres aléatoires dans les cases vides restantes.
* **Affichage de la grille:** La fonction `display-grid` affiche la grille de Sudoku dans la console.

**Exemple d'Utilisation:**

```scheme
(define puzzle (generate-puzzle 30))
(display-grid puzzle)
```

Cela générera une grille de Sudoku partiellement résolue avec 30 indices donnés et l'affichera dans la console.