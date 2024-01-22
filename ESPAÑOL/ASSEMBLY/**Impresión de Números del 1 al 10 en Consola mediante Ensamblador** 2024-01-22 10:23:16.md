```assembly
; Este programa imprimirá los números del 1 al 10 en la consola.

; Sección de datos
datos:
    ; Define una cadena de caracteres que contiene el mensaje a imprimir.
    mensaje: db "Números del 1 al 10:", 10

; Sección de código
código:
    ; Inicio del programa.
inicio:
        ; Guarda el segmento de datos en el registro DS.
        mov ax, datos
        mov ds, ax

        ; Guarda el segmento de código en el registro CS.
        mov ax, código
        mov cs, ax

        ; Imprime el mensaje en la consola.
        mov ah, 9
        mov dx, mensaje
        int 21h

        ; Pausa el programa hasta que el usuario pulse una tecla.
        mov ah, 1
        int 21h

        ; Termina el programa.
        mov ah, 4ch
        mov al, 0
        int 21h

; Fin del programa.
```

Explicación del código:

* La sección de datos contiene una cadena de caracteres que contiene el mensaje a imprimir.
* La sección de código contiene el código del programa.
* El inicio del programa es el punto de entrada del programa.
* El programa guarda el segmento de datos en el registro DS y el segmento de código en el registro CS.
* El programa imprime el mensaje en la consola utilizando la llamada al sistema `int 21h` con el código de función 9.
* El programa pausa el programa hasta que el usuario pulse una tecla utilizando la llamada al sistema `int 21h` con el código de función 1.
* El programa termina el programa utilizando la llamada al sistema `int 21h` con el código de función 4ch.