```scheme
(define-struct persona (nombre edad altura))

(define (crear-persona nombre edad altura)
  (make-persona nombre edad altura))

(define (nombre-de persona)
  (persona-nombre persona))

(define (edad-de persona)
  (persona-edad persona))

(define (altura-de persona)
  (persona-altura persona))

(define (es-mayor-de-edad? persona)
  (> (edad-de persona) 18))

(define (es-menor-de-edad? persona)
  (< (edad-de persona) 18))

(define (imprimir-persona persona)
  (display "Nombre: " (nombre-de persona))
  (newline)
  (display "Edad: " (edad-de persona))
  (newline)
  (display "Altura: " (altura-de persona))
  (newline))

(define-struct libro (titulo autor año-de-publicación))

(define (crear-libro titulo autor año-de-publicación)
  (make-libro titulo autor año-de-publicación))

(define (titulo-de libro)
  (libro-titulo libro))

(define (autor-de libro)
  (libro-autor libro))

(define (año-de-publicación-de libro)
  (libro-año-de-publicación libro))

(define (imprimir-libro libro)
  (display "Título: " (titulo-de libro))
  (newline)
  (display "Autor: " (autor-de libro))
  (newline)
  (display "Año de publicación: " (año-de-publicación-de libro))
  (newline))

(define-struct biblioteca (nombre libros))

(define (crear-biblioteca nombre)
  (make-biblioteca nombre '()))

(define (nombre-de biblioteca)
  (biblioteca-nombre biblioteca))

(define (libros-de biblioteca)
  (biblioteca-libros biblioteca))

(define (añadir-libro biblioteca libro)
  (set-biblioteca-libros biblioteca (cons libro (libros-de biblioteca))))

(define (quitar-libro biblioteca libro)
  (set-biblioteca-libros biblioteca (remove libro (libros-de biblioteca))))

(define (imprimir-biblioteca biblioteca)
  (display "Nombre: " (nombre-de biblioteca))
  (newline)
  (display "Libros:")
  (newline)
  (for-each (lambda (libro)
              (imprimir-libro libro))
            (libros-de biblioteca)))

(define biblioteca-central (crear-biblioteca "Biblioteca Central"))
(define libro1 (crear-libro "El Quijote" "Miguel de Cervantes" 1605))
(define libro2 (crear-libro "Cien años de soledad" "Gabriel García Márquez" 1967))
(define persona1 (crear-persona "Juan" 20 180))
(define persona2 (crear-persona "María" 15 160))

(añadir-libro biblioteca-central libro1)
(añadir-libro biblioteca-central libro2)

(imprimir-persona persona1)
(newline)
(imprimir-persona persona2)
(newline)
(imprimir-biblioteca biblioteca-central)
```

Este código crea una biblioteca con dos libros, una persona adulta y una persona menor de edad. Luego imprime la información de la biblioteca, la persona adulta y la persona menor de edad.

Aquí se usan structs, procedimientos y estructuras de control de flujo como bucles y condicionales para crear una aplicación compleja y funcional en SCHEME.