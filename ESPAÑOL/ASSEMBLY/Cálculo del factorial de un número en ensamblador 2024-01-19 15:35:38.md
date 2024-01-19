```assembly
; Programa ensamblador para calcular el factorial de un número
.model small
.stack 100h
.data
numero db 5
factorial dw 0
; Código ensamblador para calcular el factorial
.code
inicio:
    mov ax, @data
    mov ds, ax

    ; Calculamos el factorial del número 5
    mov bx, numero
    mov cx, 0
    factorial_loop:
        inc cx
        mov ax, bx
        mul cx
        mov factorial, ax
        loop factorial_loop

    ; Mostramos el factorial en pantalla
    mov ax, 4096
    mov ds, ax
    mov ah, 9
    mov dx, offset mensaje
    int 21h

    ; Esperamos a que el usuario presione una tecla
    ; para finalizar el programa
    mov ah, 1
    int 21h

    ; Finalizamos el programa
    mov ax, 4C00h
    int 21h

; Mensaje que se muestra en pantalla
.data
mensaje db "El factorial de ", numero, " es: ", factorial, "$"
```

Explicación del código:

* La primera línea del código es la directiva `.model small`, que le indica al ensamblador que el programa que estamos escribiendo es un programa pequeño.
* La segunda línea del código es la directiva `.stack 100h`, que le indica al ensamblador que queremos reservar 100 bytes de memoria para la pila.
* La tercera línea del código es la directiva `.data`, que le indica al ensamblador que vamos a definir una sección de datos.
* La cuarta línea del código define una variable llamada `numero` con el valor 5.
* La quinta línea del código define una variable llamada `factorial` con el valor 0.
* La sexta línea del código es la directiva `.code`, que le indica al ensamblador que vamos a definir una sección de código.
* La séptima línea del código es la etiqueta `inicio`, que indica el inicio del programa.
* La octava línea del código mueve el segmento de datos a la dirección de memoria @data.
* La novena línea del código mueve el segmento de código a la dirección de memoria @code.
* La décima línea del código inicializa la variable `bx` con el valor de la variable `numero`.
* La undécima línea del código inicializa la variable `cx` con el valor 0.
* La duodécima línea del código es la etiqueta `factorial_loop`, que indica el inicio del bucle que calcula el factorial.
* La decimotercera línea del código incrementa el valor de la variable `cx` en 1.
* La decimocuarta línea del código multiplica el valor de la variable `bx` por el valor de la variable `cx`.
* La decimoquinta línea del código mueve el resultado de la multiplicación a la variable `factorial`.
* La decimosexta línea del código hace un bucle de vuelta a la etiqueta `factorial_loop` hasta que el valor de la variable `cx` sea mayor que el valor de la variable `bx`.
* La decimoséptima línea del código mueve el segmento de datos a la dirección de memoria 4096.
* La decimoctava línea del código mueve el registro `ah` al valor 9.
* La decimonovena línea del código mueve el registro `dx` a la dirección de memoria del mensaje.
* La vigésima línea del código llama a la interrupción 21h, que muestra el mensaje en pantalla.
* La vigesimoprimera línea del código espera a que el usuario presione una tecla.
* La vigesimosegunda línea del código finaliza el programa.
* La vigesimotercera línea del código es la directiva `.data`, que le indica al ensamblador que vamos a definir una sección de datos.
* La vigesimocuarta línea del código define una variable llamada `mensaje` con el valor "El factorial de ", número, " es: ", factorial, "$".