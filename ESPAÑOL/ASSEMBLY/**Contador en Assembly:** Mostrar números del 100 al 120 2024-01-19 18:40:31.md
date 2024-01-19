```assembly
; Definir el segmento de datos
.data
numero: .byte 100 ; Declarar una variable de 8 bits llamada "numero" e inicializarla en 100

; Definir el segmento de código
.code
inicio:
    ; Mostrar el valor de la variable "numero" en la consola
    mov al, [numero]    ; Copiar el valor de "numero" en el registro AL
    mov ah, 02h         ; Establecer el código de función para "mostrar carácter"
    int 21h            ; Interrumpir el sistema para mostrar el carácter

    ; Incrementar el valor de la variable "numero" en 1
    inc [numero]        ; Incrementar el valor de "numero" en 1

    ; Comprobar si el valor de "numero" es igual a 120
    cmp [numero], 120h  ; Comparar el valor de "numero" con 120h
    je fin             ; Si son iguales, saltar a la etiqueta "fin"

    ; Si no son iguales, volver a la etiqueta "inicio"
    jmp inicio

fin:
    ; Mostrar el mensaje de fin de programa en la consola
    mov ah, 09h         ; Establecer el código de función para "mostrar cadena"
    mov dx, mensaje     ; Cargar la dirección del mensaje en el registro DX
    int 21h            ; Interrumpir el sistema para mostrar el mensaje

    ; Terminar el programa
    mov ah, 4Ch         ; Establecer el código de función para "terminar programa"
    int 21h            ; Interrumpir el sistema para terminar el programa

mensaje: .asciz "Programa terminado" ; Declarar una cadena de caracteres llamada "mensaje"
```

Explicación:

* La directiva `.data` define el segmento de datos del programa, donde se almacenan las variables.
* La directiva `.code` define el segmento de código del programa, donde se encuentran las instrucciones que se ejecutan.
* La etiqueta `inicio:` define el punto de entrada del programa.
* La instrucción `mov al, [numero]` copia el valor de la variable "numero" en el registro AL.
* La instrucción `mov ah, 02h` establece el código de función para "mostrar carácter" en el registro AH.
* La instrucción `int 21h` interrumpe el sistema para mostrar el carácter en la consola.
* La instrucción `inc [numero]` incrementa el valor de la variable "numero" en 1.
* La instrucción `cmp [numero], 120h` compara el valor de la variable "numero" con 120h.
* La instrucción `je fin` salta a la etiqueta "fin" si el valor de "numero" es igual a 120h.
* La instrucción `jmp inicio` vuelve a la etiqueta "inicio" si el valor de "numero" no es igual a 120h.
* La etiqueta `fin:` define el punto de salida del programa.
* La instrucción `mov ah, 09h` establece el código de función para "mostrar cadena" en el registro AH.
* La instrucción `mov dx, mensaje` carga la dirección del mensaje en el registro DX.
* La instrucción `int 21h` interrumpe el sistema para mostrar el mensaje en la consola.
* La instrucción `mov ah, 4Ch` establece el código de función para "terminar programa" en el registro AH.
* La instrucción `int 21h` interrumpe el sistema para terminar el programa.
* La directiva `.asciz "Programa terminado"` declara una cadena de caracteres llamada "mensaje".