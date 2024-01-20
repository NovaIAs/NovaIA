```scheme
(define (suma-lista lista)
  (if (null? lista)
      0
      (+ (car lista) (suma-lista (cdr lista)))))

(define (promedio lista)
  (/ (suma-lista lista) (length lista)))

(define (mediana lista)
  (let ((ordenada (sort lista)))
      (if (even? (length ordenada))
          (/ (+ (car (cdr ordenada)) (cadr ordenada)) 2)
          (caar ordenada))))

(define (moda lista)
  (let ((frecuencias (frequencies lista)))
      (max-value (map car (assocs (max-value (map cadr frecuencias)) frecuencias)))))

(define (desviacion-estandar lista)
  (let ((media (promedio lista)))
      (sqrt (/ (sum (map (lambda (x) (sqr (- x media))) lista))
                (- (length lista) 1))))))

(define (intervalo-confianza lista nivel-confianza)
  (let ((media (promedio lista))
        (desviacion (desviacion-estandar lista)))
      (+ media (* desviacion
                   (quotient (/ (quantile nivel-confianza)
                               (- (length lista) 1))
                            2))))))
```

Este código en SCHEME calcula las siguientes estadísticas descriptivas de una lista de números:

* Suma
* Promedio
* Mediana
* Moda
* Desviación estándar
* Intervalo de confianza

El código está dividido en varias funciones:

* `suma-lista`: Calcula la suma de los elementos de una lista.
* `promedio`: Calcula el promedio de los elementos de una lista.
* `mediana`: Calcula la mediana de los elementos de una lista.
* `moda`: Calcula la moda de los elementos de una lista.
* `desviacion-estandar`: Calcula la desviación estándar de los elementos de una lista.
* `intervalo-confianza`: Calcula el intervalo de confianza de los elementos de una lista.

Para usar este código, simplemente llamas a la función que necesites con la lista de números como argumento. Por ejemplo, para calcular la suma de la lista `[1, 2, 3]`, usarías el siguiente código:

```scheme
(suma-lista '(1 2 3))
```

El resultado sería `6`.

Este código puede ser útil para analizar datos estadísticos. Por ejemplo, puedes usar `intervalo-confianza` para determinar si la diferencia entre dos conjuntos de datos es estadísticamente significativa.