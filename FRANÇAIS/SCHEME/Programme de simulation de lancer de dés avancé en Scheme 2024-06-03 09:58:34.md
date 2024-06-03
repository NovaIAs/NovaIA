**Programme de simulation de lancer de dés complexe en Scheme**

**(Définitions)**

```scheme
(define (lancer-des) (random 6) + 1)

(define (lancer-liste-des n) (map lancer-des (make-list n)))

(define (lancer-nombre-des n m) (car (filter (lambda (x) (= x m)) (lancer-liste-des n))))

(define (somme-des-lancers n) (apply + (lancer-liste-des n)))

(define (moyenne-des-lancers n) (/ (somme-des-lancers n) n))

(define (ecart-type-des-lancers n)
  (sqrt (/ (- (moyenne-des-lancers n) ^ 2 (variance-des-lancers n)) n)))

(define (variance-des-lancers n)
  (/ (sum-of-squares-of-deviations-des-lancers n) (- n 1)))

(define (sum-of-squares-of-deviations-des-lancers n)
  (let loop ([acc 0] [i 0])
    (when (< i n)
      (define deviance (- (car (lancer-liste-des n i))
                            (moyenne-des-lancers n)))
      (loop (+ acc (square deviance)) (add1 i)))))
```

**Fonction principale**

```scheme
(define (simuler-lancers n)
  (let loop ([i 0])
    (when (< i n)
      (display (format tilde (~a~%) (lancer-des)))
      (loop (add1 i)))))
```

**Utilisation**

```scheme
(simuler-lancers 10)
```

**Explication**

* La fonction `lancer-des` lance un dé à six faces.
* La fonction `lancer-liste-des` lance une liste de n dés.
* La fonction `lancer-nombre-des` lance n dés et renvoie le nombre de dés ayant donné un résultat spécifique.
* La fonction `somme-des-lancers` calcule la somme des résultats d'une liste de lancers de dés.
* La fonction `moyenne-des-lancers` calcule la moyenne des résultats d'une liste de lancers de dés.
* La fonction `ecart-type-des-lancers` calcule l'écart-type des résultats d'une liste de lancers de dés.
* La fonction `variance-des-lancers` calcule la variance des résultats d'une liste de lancers de dés.
* La fonction `sum-of-squares-of-deviations-des-lancers` calcule la somme des carrés des écarts par rapport à la moyenne pour une liste de lancers de dés.
* La fonction `simuler-lancers` simule n lancers de dés et affiche les résultats.