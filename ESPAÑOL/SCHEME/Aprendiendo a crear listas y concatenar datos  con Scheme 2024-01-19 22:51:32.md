```scheme
(define variable1 "Hola")
(define variable2 "hago")
(define variable3 "cursos")
(define variable4 "en")
(define variable5 "Platzi")

(define listar-variables (lambda ()
  (display variable1)
  (display variable2)
  (display variable3)
  (display variable4)
  (display variable5)
  (newline)))

(listar-variables)

(display (string-append "Aprendiendo " variable1))
(newline)
(display (string-append "Aprendiendo " variable2))
(newline)
(display (string-append "Aprendiendo " variable3))
(newline)
(display (string-append "Aprendiendo " variable4))
(newline)
(display (string-append "Aprendiendo " variable5))
(newline)
```

El código anterior es un programa relativamente complejo en Scheme. Crea y define cinco variables, las cuales se pueden utilizar en cualquier parte del programa.

Las variables son:

* **variable1:** Hola
* **variable2:** hago
* **variable3:** cursos
* **variable4:** en
* **variable5:** Platzi

La función **listar-variables** lista las cinco variables en la pantalla.

Luego, el programa utiliza la función **string-append** para crear cinco nuevas cadenas de caracteres, cada una de las cuales consiste en la palabra "Aprendiendo" seguida de una de las variables. Estas nuevas cadenas de caracteres se muestran en la pantalla.

Este programa es más complejo que la mayoría de los programas de Scheme que se encuentran en línea, pero aún es relativamente sencillo de entender. Muestra cómo crear y utilizar variables, funciones y cadenas de caracteres en Scheme.

También demuestra cómo utilizar la función **display** para mostrar datos en la pantalla.