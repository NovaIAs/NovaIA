```scheme
(define (facto n)
  (if (zero? n)
      1
      (* n (facto (- n 1)))))

(define (combinaison n p)
  (/ (facto n) (* (facto p) (facto (- n p)))))

(define (permuter n p)
  (/ (facto n) (facto (- n p))))

(define (binome-newton n p)
  (cond
    [(or (< n 0) (< p 0) (< p n))
     #f]
    [(= p n)
     1]
    [(= p 0)
     1]
    [else
     (+ (binome-newton (- n 1) (- p 1))
        (binome-newton (- n 1) p))]))

(define (somme-serie-geo-finie a r n)
  (if (zero? r)
      (* a n)
      (/ (- a (* r (expt r n)))
         (- 1 r))))

(define (somme-serie-geo-infinie a r)
  (if (= r 1)
      #f
      (/ a (- 1 r))))

(define (somme-serie-harmonique n)
  (cond
    [(= n 0)
     0]
    [(< n 0)
     #f]
    [else
     (+ n (somme-serie-harmonique (- n 1))))))

(define (suite-syracuse n)
  (cond
    [(odd? n)
     (* 3 n 1)]
    [else
     (/ n 2)]))
```

**Explications du code :**

* **Facto :** Calcule la factorielle d'un nombre.
* **Combinaison :** Calcule le nombre de combinaisons de n éléments pris p à p.
* **Permuter :** Calcule le nombre de permutations de n éléments pris p à p.
* **Binome-newton :** Calcule le coefficient binomial de n et p.
* **Somme-serie-geo-finie :** Calcule la somme des n premiers termes d'une série géométrique de raison r et de premier terme a.
* **Somme-serie-geo-infinie :** Calcule la somme d'une série géométrique infinie de raison r et de premier terme a, si r est différent de 1.
* **Somme-serie-harmonique :** Calcule la n-ième somme partielle de la série harmonique.
* **Suite-syracuse :** Génère la suite de Syracuse à partir d'un entier n.