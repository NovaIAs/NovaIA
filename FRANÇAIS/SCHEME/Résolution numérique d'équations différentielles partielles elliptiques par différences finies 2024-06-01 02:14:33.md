**Résolution d'équations différentielles partielles elliptiques**

```scheme
(define (solve-pde f dt dx dy)
  (let ((n (* dx dy)))
    (define (build-matrix)
      (make-vector n
        (lambda (i)
          (make-vector n
            (lambda (j)
              (- (if (and (= 0 i) (= 0 j))
                    (* 4 f 0 0)
                    (* 2 f i dx 0))
                 (if (= i 0)
                   (* f j dx 0)
                   (* f i dx j dx))
                 (if (= 0 j)
                   (* f 0 dy 0)
                   (* f i dx 0 dy))
                 (* dt (if (and (= 0 i) (= 0 j)) 2 (/ 1 (* dx dx)) 1 (/ 1 (* dx dx dy)) 1 (/ 1 (* dy dy)))
                      (* (if (= 0 i) 0 f i dx 0) j dx 0)
                      (* (if (= 0 i) 0 f i dx 0) 0 dy 0)
                      (* (if (= 0 j) 0 f 0 dy 0) i dx 0)
                      (* (if (= 0 j) 0 f 0 dy 0) 0 dx j dy))))))))))

    (define (row-scale row i scale)
      (for/list ((j i n))
        (set! (ref row j) (* scale (ref row j)))))

    (define (add-rows dest src i scale)
      (for/list ((j i n))
        (add-values! (ref dest j) (* scale (ref src j)))))

    (define (eliminate-row m i)
      (for/list ((j (add1 i) n))
        (define scale (- (ref m j i)))
        (when (not (zero? scale))
          (row-scale m i scale)
          (add-rows m m j scale)))))

    (define (solve-matrix m x)
      (for/list ((i (- n 1) -1 -1))
        (for/list ((j i (- n 1) -1 -1))
          (when (not (zero? (ref m i j)))
            (add-values! (ref m i n) (* (ref m i j) (ref x j))))
          (eliminate-row m i))))
      (for/list ((i 0 n))
        (set! (ref x i) (/ (ref m i n) (ref m i i)))))

    (define (gradient x)
      (make-vector n
        (lambda (i)
          (/ (- (ref x (add1 i)) (ref x (sub1 i))) dx)))))

    (define (divergence x)
      (apply + (gradient x)))

    (define m (build-matrix))
    (define x (make-vector n 0))
    (solve-matrix m x)

    (cons x (divergence x))))
```

**Explication :**

Cette fonction résout des équations différentielles partielles elliptiques de la forme :

```
-dt * lap(u(x,y)) + f(x,y) = 0
```

Elle utilise la méthode des différences finies et un solveur matriciel pour trouver la solution `u(x,y)` sur un maillage rectangulaire.

Le code commence par construire la matrice `m` qui représente le système d'équations linéaires à résoudre.

Ensuite, il utilise une combinaison de mises à l'échelle de lignes, d'ajouts de lignes et d'éliminations de lignes pour résoudre le système et obtenir le vecteur de solution `x`.

Enfin, la fonction calcule le gradient et la divergence de la solution.