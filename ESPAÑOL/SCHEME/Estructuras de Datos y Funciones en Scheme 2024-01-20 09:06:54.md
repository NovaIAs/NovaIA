```scheme
; Definiciones de estructuras

(define-struct persona (nombre edad))
(define-struct direccion (calle ciudad codigo_postal))
(define-struct libro (titulo autor año))

; Funciones para crear objetos

(define (crear-persona nombre edad)
  (make-persona :nombre nombre :edad edad))

(define (crear-direccion calle ciudad codigo_postal)
  (make-direccion :calle calle :ciudad ciudad :codigo_postal codigo_postal))

(define (crear-libro titulo autor año)
  (make-libro :titulo titulo :autor autor :año año))

; Funciones para obtener datos de los objetos

(define (obtener-nombre persona)
  (person-ref persona 'nombre))

(define (obtener-edad persona)
  (person-ref persona 'edad))

(define (obtener-calle direccion)
  (direccion-ref direccion 'calle))

(define (obtener-ciudad direccion)
  (direccion-ref direccion 'ciudad))

(define (obtener-codigo_postal direccion)
  (direccion-ref direccion 'codigo_postal))

(define (obtener-titulo libro)
  (libro-ref libro 'titulo))

(define (obtener-autor libro)
  (libro-ref libro 'autor))

(define (obtener-año libro)
  (libro-ref libro 'año))

; Funciones para imprimir datos de los objetos

(define (imprimir-persona persona)
  (displayln "Nombre: " (obtener-nombre persona)))

(define (imprimir-direccion direccion)
  (displayln "Calle: " (obtener-calle direccion)))

(define (imprimir-ciudad direccion)
  (displayln "Ciudad: " (obtener-ciudad direccion)))

(define (imprimir-codigo_postal direccion)
  (displayln "Código postal: " (obtener-código_postal dirección)))

(define (imprimir-libro libro)
  (displayln "Título: " (obtener-título libro)))

(define (imprimir-autor libro)
  (displayln "Autor: " (obtener-autor libro)))

(define (imprimir-año libro)
  (displayln "Año: " (obtener-año libro)))

; Función principal

(define (main)
  (let ((persona (crear-persona "Juan" 25))
        (direccion (crear-direccion "Calle Mayor" "Madrid" 28013))
        (libro (crear-libro "El Quijote" "Miguel de Cervantes" 1605)))
    (imprimir-persona persona)
    (imprimir-direccion dirección)
    (imprimir-código_postal dirección)
    (imprimir-libro libro)
    (imprimir-autor libro)
    (imprimir-año libro)))

(main)
```

Explicación del código:

* Definición de las estructuras `persona`, `dirección` y `libro`. Esto permite agrupar datos relacionados en un solo objeto.
* Definición de funciones para crear objetos de cada tipo, `crear-persona`, `crear-dirección` y `crear-libro`.
* Definición de funciones para obtener datos de los objetos, `obtener-nombre`, `obtener-edad`, `obtener-calle`, `obtener-ciudad`, `obtener-código_postal`, `obtener-título`, `obtener-autor` y `obtener-año`.
* Definición de funciones para imprimir datos de los objetos, `imprimir-persona`, `imprimir-dirección`, `imprimir-código_postal`, `imprimir-libro`, `imprimir-autor` y `imprimir-año`.
* Función `main` que crea objetos de cada tipo, y llama a las funciones para imprimir sus datos.