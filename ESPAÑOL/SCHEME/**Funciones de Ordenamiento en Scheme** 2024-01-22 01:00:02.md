```scheme
; Define una función que toma una lista y devuelve una lista con los elementos en orden inverso.
(define (invertir lista)
  (cond
    ((null? lista) '())
    (else (append (invertir (cdr lista)) (list (car lista))))))

; Define una función que toma una lista y devuelve una lista con los elementos en orden aleatorio.
(define (aleatorio lista)
  (shuffle (copy-list lista)))

; Define una función que toma una lista y devuelve una lista con los elementos en orden ascendente.
(define (ordenar lista)
  (sort lista <))

; Define una función que toma una lista y devuelve una lista con los elementos en orden descendente.
(define (ordenar-descendente lista)
  (sort lista >))

; Define una función que toma una lista y devuelve una lista con los elementos en orden alfabético.
(define (ordenar-alfabetico lista)
  (sort lista string<?))

; Define una función que toma una lista y devuelve una lista con los elementos en orden alfabético inverso.
(define (ordenar-alfabetico-inverso lista)
  (sort lista string?>))

; Define una función que toma una lista y devuelve una lista con los elementos en orden numérico.
(define (ordenar-numerico lista)
  (sort lista number<?))

; Define una función que toma una lista y devuelve una lista con los elementos en orden numérico inverso.
(define (ordenar-numerico-inverso lista)
  (sort lista number?>))

```

Explicación:

* La función `invertir` toma una lista y devuelve una lista con los elementos en orden inverso.
* La función `aleatorio` toma una lista y devuelve una lista con los elementos en orden aleatorio.
* La función `ordenar` toma una lista y devuelve una lista con los elementos en orden ascendente.
* La función `ordenar-descendente` toma una lista y devuelve una lista con los elementos en orden descendente.
* La función `ordenar-alfabetico` toma una lista y devuelve una lista con los elementos en orden alfabético.
* La función `ordenar-alfabetico-inverso` toma una lista y devuelve una lista con los elementos en orden alfabético inverso.
* La función `ordenar-numerico` toma una lista y devuelve una lista con los elementos en orden numérico.
* La función `ordenar-numerico-inverso` toma una lista y devuelve una lista con los elementos en orden numérico inverso.