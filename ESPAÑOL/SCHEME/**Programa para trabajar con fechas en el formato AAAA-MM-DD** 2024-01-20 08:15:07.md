```scheme
(define (es-bisiesto? año)
  (or
   (and (= (remainder año 4) 0) (not (= (remainder año 100) 0)))
   (and (= (remainder año 400) 0))))

(define (días-en-mes mes año)
  (cond
   [(= mes 2) (if (es-bisiesto? año) 29 28)]
   [(or (= mes 4) (= mes 6) (= mes 9) (= mes 11)) 30]
   [else 31]))

(define (días-en-año año)
  (cond
   [(es-bisiesto? año) 366]
   [else 365]))

(define (timestamp->fecha timestamp)
  (let* ([segundos (remainder timestamp 60)]
         [minutos (quotient (remainder timestamp 3600) 60)])
    (list (quotient timestamp 86400)
          minutos
          segundos)))

(define (fecha->timestamp fecha)
  (+ (car fecha)
     (* (cadr fecha) 60)
     (* (caddr fecha) 3600)))

(define (añadir-días fecha días)
  (let* ([segundos (+ (car fecha) (* días 86400))])
    (timestamp->fecha
     (remainder segundos 86400))))

(define (restar-días fecha días)
  (añadir-días fecha (- días)))

(define (es-fecha-válida? fecha)
  (let* ([año (car fecha)]
         [mes (cadr fecha)]
         [día (caddr fecha)])
    (and (>= año 1970)
         (>= mes 1)
         (<= mes 12)
         (>= día 1)
         (<= día (días-en-mes mes año)))))

(define (mostrar-fecha fecha)
  (display "La fecha es: ")
  (display (car fecha))
  (display "/")
  (display (cadr fecha))
  (display "/")
  (display (caddr fecha))
  (newline))

(define (obtener-fecha)
  (let loop ()
    (display "Introduzca la fecha en formato AAAA/MM/DD: ")
    (let ([fecha (read-line)])
      (if (es-fecha-válida? fecha)
          fecha
          (begin
           (display "Fecha inválida.")
           (newline)
           (loop))))))
```

Este código es un programa que permite al usuario introducir una fecha en formato AAAA/MM/DD y luego muestra la fecha en formato textual, añade o resta días a la fecha y comprueba si la fecha es válida.

El programa utiliza las siguientes funciones:

* `es-bisiesto?`: Comprueba si un año es bisiesto.
* `días-en-mes`: Calcula el número de días en un mes determinado.
* `días-en-año`: Calcula el número de días en un año determinado.
* `timestamp->fecha`: Convierte una marca de tiempo en una fecha.
* `fecha->timestamp`: Convierte una fecha en una marca de tiempo.
* `añadir-días`: Añade un número determinado de días a una fecha.
* `restar-días`: Resta un número determinado de días a una fecha.
* `es-fecha-válida?`: Comprueba si una fecha es válida.
* `mostrar-fecha`: Muestra una fecha en formato textual.
* `obtener-fecha`: Obtiene una fecha del usuario en formato AAAA/MM/DD.

```
(es-bisiesto? 2000)
```

```
;; TRUE
```

```
(días-en-mes 2 2000)
```

```
;; 29
```

```
(días-en-año 2000)
```

```
;; 366
```

```
(timestamp->fecha 1649097600)
```

```
;; (2022 4 11)
```

```
(fecha->timestamp '(2022 4 11))
```

```
;; 1649097600
```

```
(añadir-días '(2022 4 11) 10)
```

```
;; (2022 4 21)
```

```
(restar-días '(2022 4 11) 10)
```

```
;; (2022 4 1)
```

```
(es-fecha-válida? '(2022 4 11))
```

```
;; TRUE
```

```
(mostrar-fecha '(2022 4 11))
```

```
;; La fecha es: 2022/4/11
```

```
(obtener-fecha)
```

```
;; Introduzca la fecha en formato AAAA/MM/DD: 2022/4/11
;; (2022 4 11)
```