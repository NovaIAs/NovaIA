```
; Archivo: programa.asm

; Define el segmento de datos
.data
mensaje: .asciz "Hola, mundo!" ; Mensaje a imprimir
longitud: .int 13 ; Longitud del mensaje (sin incluir el carácter nulo)

; Define el segmento de código
.code
main:
    ; Inicia la pila
    mov sp, 0x1000

    ; Imprime el mensaje en la consola
    mov eax, 4 ; sys_write
    mov ebx, 1 ; stdout
    mov ecx, mensaje ; Dirección del mensaje
    mov edx, longitud ; Longitud del mensaje
    int 0x80 ; Llamada al sistema

    ; Termina el programa
    mov eax, 1 ; sys_exit
    mov ebx, 0 ; Código de salida
    int 0x80 ; Llamada al sistema

; Fin del programa
```

Explicación:

* El segmento de datos (`data`) contiene las variables globales del programa, en este caso el mensaje a imprimir y su longitud.
* El segmento de código (`code`) contiene las instrucciones que el programa ejecutará.
* La función `main` es el punto de entrada del programa.
* La instrucción `mov sp, 0x1000` inicializa la pila en la dirección 0x1000.
* La instrucción `mov eax, 4` establece el registro `eax` en el valor 4, que es el número de la llamada al sistema `sys_write`.
* La instrucción `mov ebx, 1` establece el registro `ebx` en el valor 1, que es el descriptor de fichero estándar para la consola.
* La instrucción `mov ecx, mensaje` establece el registro `ecx` en la dirección del mensaje a imprimir.
* La instrucción `mov edx, longitud` establece el registro `edx` en la longitud del mensaje.
* La instrucción `int 0x80` llama a la llamada al sistema `sys_write`, que imprimirá el mensaje en la consola.
* La instrucción `mov eax, 1` establece el registro `eax` en el valor 1, que es el número de la llamada al sistema `sys_exit`.
* La instrucción `mov ebx, 0` establece el registro `ebx` en el valor 0, que es el código de salida del programa.
* La instrucción `int 0x80` llama a la llamada al sistema `sys_exit`, que terminará el programa.