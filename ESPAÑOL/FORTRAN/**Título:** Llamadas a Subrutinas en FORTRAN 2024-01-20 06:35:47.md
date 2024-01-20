```fortran
 PROGRAMA PRINCIPAL

 IMPRIME, 'INICIA PROGRAMA PRINCIPAL'

 LLAMA SUBRUTINA1

 LLAMA SUBRUTINA2

 LLAMA SUBRUTINA3

 IMPRIME, 'FINALIZA PROGRAMA PRINCIPAL'

 FIN

 SUBRUTINA1

 IMPRIME, 'INICIA SUBRUTINA1'

 LLAMA SUBRUTINA2

 LLAMA SUBRUTINA3

 IMPRIME, 'FINALIZA SUBRUTINA1'

 FIN

 SUBRUTINA2

 IMPRIME, 'INICIA SUBRUTINA2'

 LLAMA SUBRUTINA3

 IMPRIME, 'FINALIZA SUBRUTINA2'

 FIN

 SUBRUTINA3

 IMPRIME, 'INICIA SUBRUTINA3'

 IMPRIME, 'FINALIZA SUBRUTINA3'

 FIN
```

Explicación:

Este es un programa en FORTRAN que consta de un programa principal y tres subrutinas.

* El programa principal llama a las tres subrutinas en orden.

* La subrutina1 llama a las subrutinas2 y 3.

* La subrutina2 llama a la subrutina3.

* La subrutina3 no llama a ninguna otra subrutina.

Cuando se ejecuta el programa, se imprimen los siguientes mensajes:

* INICIA PROGRAMA PRINCIPAL
* INICIA SUBRUTINA1
* INICIA SUBRUTINA2
* INICIA SUBRUTINA3
* FINALIZA SUBRUTINA3
* FINALIZA SUBRUTINA2
* FINALIZA SUBRUTINA1
* FINALIZA PROGRAMA PRINCIPAL

El orden en que se imprimen los mensajes muestra la estructura del programa y el orden en que se llaman las subrutinas.