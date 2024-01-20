```asm
; Define la sección de datos
.data
mensaje: .asciiz "Hola, mundo!"
longitud_mensaje: .equ . - mensaje

; Define la sección de código
.text
.global main
main:

    ; Muestra el mensaje en la consola
    mov eax, 4
    mov ebx, 1
    mov ecx, mensaje
    mov edx, longitud_mensaje
    int 0x80

    ; Termina el programa
    mov eax, 1
    mov ebx, 0
    int 0x80
```

Explicación:

* La sección de datos define la variable mensaje, que contiene el texto "Hola, mundo!".
* La sección de código define la función principal, que es el punto de entrada del programa.
* La instrucción `mov` se utiliza para mover datos de un registro a otro. En este caso, se mueve el número 4 (el código de la llamada al sistema para imprimir una cadena de caracteres) en el registro `eax`.
* La instrucción `mov` se utiliza de nuevo para mover la dirección de la variable `mensaje` en el registro `ecx`.
* La instrucción `mov` se utiliza de nuevo para mover la longitud de la cadena de caracteres en el registro `edx`.
* La instrucción `int` se utiliza para llamar a una interrupción del sistema. En este caso, se llama a la interrupción del sistema 0x80, que es la interrupción del sistema para imprimir una cadena de caracteres.
* La instrucción `mov` se utiliza de nuevo para mover el número 1 (el código de la llamada al sistema para terminar el programa) en el registro `eax`.
* La instrucción `mov` se utiliza de nuevo para mover el número 0 (el código de retorno del programa) en el registro `ebx`.
* La instrucción `int` se utiliza de nuevo para llamar a la interrupción del sistema 0x80, que es la interrupción del sistema para terminar el programa.