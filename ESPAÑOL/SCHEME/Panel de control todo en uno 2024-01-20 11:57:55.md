```scheme
(modulo todo-en-uno
  (requiere bibliotecas
    #%diseñador.diseñador
    #%diseñador.esquema-html)
  (define (main)
    (panel-de-control #t)))

(define (panel-de-control? activar-desactivar?)
  (si activar-desactivar?
    (begin
      (:panel-de-control
        (:encabezado "Panel de control")
        (:lista-de-opciones
          (:enlace "Página principal" "/index.html")
          (:enlace "Acerca de mí" "/about.html")
          (:enlace "Contacto" "/contact.html"))
        (:pie-de-página "Copyright 2023"))
      #t)
    #f))

(define (panel-de-control tipo)
  (lambda ()
    (panel-de-control?
      (si (= 'activado tipo)
        #t
        #f))))
```