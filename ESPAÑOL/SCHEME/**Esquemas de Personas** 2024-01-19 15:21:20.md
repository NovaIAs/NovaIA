```scheme
(define-struct estructura (nombre edad ocupacion) #:transparent)

(define (crear-persona nombre edad ocupacion)
  (make-estructura 'estructura nombre edad ocupacion))

(define (obtener-nombre persona)
  (car persona))

(define (obtener-edad persona)
  (cadr persona))

(define (obtener-ocupacion persona)
  (caddr persona))

(define (imprimir-persona persona)
  (display "Nombre: " (obtener-nombre persona))
  (newline)
  (display "Edad: " (obtener-edad persona))
  (newline)
  (display "Ocupacion: " (obtener-ocupacion persona)))

(define personas '(
  (crear-persona "Juan" 25 "Ingeniero")
  (crear-persona "Maria" 30 "Maestra")
  (crear-persona "Pedro" 35 "Doctor")))

(for-each imprimir-persona personas)
```

Este código en Scheme crea una estructura personalizada llamada `estructura` que tiene tres campos: `nombre`, `edad` y `ocupacion`. Luego define varias funciones que trabajan con esta estructura, como `crear-persona`, `obtener-nombre`, `obtener-edad`, `obtener-ocupacion` e `imprimir-persona`.

La función `crear-persona` toma tres argumentos: `nombre`, `edad` y `ocupacion` y devuelve una nueva instancia de la estructura `estructura`. Las funciones `obtener-nombre`, `obtener-edad` y `obtener-ocupacion` toman una instancia de la estructura `estructura` y devuelven el valor del campo correspondiente. La función `imprimir-persona` toma una instancia de la estructura `estructura` y la imprime en la pantalla.

El código también define una lista llamada `personas` que contiene tres instancias de la estructura `estructura`. La función `for-each` itera sobre la lista `personas` y llama a la función `imprimir-persona` para cada instancia de la estructura `estructura`.

Este código es un ejemplo de cómo crear y utilizar una estructura personalizada en Scheme. Las estructuras son una forma útil de organizar y almacenar datos en Scheme.