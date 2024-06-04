```scheme

(define (is-prime? n)
  (cond ((= n 2) #t)
        ((even? n) #f)
        (else (for-all (lambda (i) (not (zero? (modulo n i))))
                          (range 3 (sqrt n) 2))))))

(define (fast-fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (fast-fib (- n 1)) + (fast-fib (- n 2))))))

(define (count-primes n)
  (for-fold (+ 0) (lambda (acc i) (if (is-prime? i) (+ acc 1) acc)) 2 n))

(define (inverse-sequence s)
  (for-fold (list) (lambda (acc x) (cons x acc)) s (list)))

(define (transpose m)
  (for-each (lambda (r) (display (for-each (lambda (c) (display c)) r)))
            (for-fold (list) (lambda (acc r) (append acc (list-map car r))) m (list))))

(define (binary-search xs x)
  (define (bin-search xs x low high)
    (let ((mid (floor (/ (+ low high) 2))))
      (if (eqv? x (list-ref xs mid)) mid
          (if (> x (list-ref xs mid))
              (bin-search xs x (+ mid 1) high)
              (bin-search xs x low (- mid 1))))))
  (if (is-sorted? xs)
      (bin-search xs x 0 (- (length xs) 1))
      #f))

(define (has-duplicate? xs)
  (let ((ys (remove-duplicates xs)))
    (not (= (length xs) (length ys)))))

```

**Ce code implémente les fonctions suivantes en Scheme :**

* `is-prime?` : Détermine si un entier est un nombre premier.
* `fast-fib` : Calcule le n-ième nombre de Fibonacci de manière récursive rapide.
* `count-primes` : Compte le nombre de nombres premiers jusqu'à un entier donné.
* `inverse-sequence` : Inverse une séquence donnée.
* `transpose` : Transpose une matrice donnée.
* `binary-search` : Effectue une recherche binaire dans une liste triée.
* `has-duplicate?` : Détermine si une liste donnée contient des doublons.