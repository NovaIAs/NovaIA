(define (nombre-completo (nombre1 nombre2 apellido1 apellido2)
  (string-append (string-append nombre1 " ") apellido1))

(define (edad (fecha-nacimiento)
  (list (quotient (- (current-date) fecha-nacimiento) 365)
        (- (modulo (- (current-date) fecha-nacimiento) 365)
            (modulo (- (current-date) (add1 fecha-nacimiento)) 365))))

(define (es-mayor-de-edad? edad)
  (> (car edad) 18))

(define (imprimir-informacion personal)
  (display (nombre-completo (car personal) (cadr personal)
                            (caddr personal) (cadddr personal)))
  (newline)
  (display (edad (caddddr personal)))
  (newline)
  (display (es-mayor-de-edad? (edad (caddddr personal)))))

(define (main)
  (imprimir-informacion '("Juan" "Pérez" "López" "García" 1980-01-01)))

(main)