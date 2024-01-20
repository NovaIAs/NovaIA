```assembly
.model small
.data
    mensaje: db '¡Hola mundo!', 13, 10, '$'
    longitud_mensaje: equ $ - mensaje

.code
    org 100h

    mov ax, @data
    mov ds, ax

    mov ax, mensaje
    mov dx, longitud_mensaje
    mov ah, 9
    int 21h

    mov ax, 4c00h
    int 21h
```

Explicación:

* Este programa muestra el mensaje "¡Hola mundo!" en la pantalla y luego termina.
* La primera línea de código especifica que el programa es un modelo pequeño, lo que significa que sólo puede acceder a 64 KB de memoria.
* La siguiente línea de código define el segmento de datos, que es donde se almacenan las variables.
* La variable `mensaje` contiene el texto "¡Hola mundo!" y los caracteres de nueva línea y fin de cadena.
* La variable `longitud_mensaje` es una constante que contiene la longitud del mensaje.
* La siguiente sección del código define el segmento de código, que es donde se encuentra el código ejecutable.
* La línea de código `org 100h` establece la dirección de inicio del segmento de código en 100h.
* La siguiente línea de código mueve el registro `ax` a la dirección del segmento de datos.
* La siguiente línea de código mueve el registro `ds` al registro `ax`, que establece el segmento de datos como el segmento de datos actual.
* La siguiente línea de código mueve el registro `ax` a la dirección del mensaje.
* La siguiente línea de código mueve el registro `dx` a la longitud del mensaje.
* La siguiente línea de código mueve el registro `ah` a 9, que es el código de la función de salida de texto.
* La siguiente línea de código llama a la interrupción 21h, que ejecuta la función de salida de texto.
* La siguiente línea de código mueve el registro `ax` a 4c00h, que es el código de la función de salida del programa.
* La siguiente línea de código llama a la interrupción 21h, que ejecuta la función de salida del programa.