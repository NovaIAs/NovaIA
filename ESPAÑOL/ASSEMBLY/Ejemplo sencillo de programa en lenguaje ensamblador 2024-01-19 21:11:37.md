```assembly
; Esta es una sección de datos que contiene las cadenas de texto que se mostrarán en pantalla.
seccion datos

mensaje1: db "Hola, mundo!", 13, 10, '$' ; Mensaje de bienvenida
mensaje2: db "Este es un programa en lenguaje ensamblador.", 13, 10, '$' ; Mensaje informativo
mensaje3: db "Presione cualquier tecla para continuar...", 13, 10, '$' ; Mensaje de despedida

; Esta es una sección de código que contiene las instrucciones que se ejecutarán.
seccion codigo

; Se carga el segmento de datos en el registro DS.
mov ax, datos ; Se carga la dirección de la sección de datos en el registro AX.
mov ds, ax ; Se copia el contenido del registro AX en el registro DS.

; Se muestra el mensaje de bienvenida en pantalla.
mov ah, 9 ; Se carga el número de la función "escribir cadena" en el registro AH.
mov dx, mensaje1 ; Se carga la dirección del mensaje de bienvenida en el registro DX.
int 21h ; Se realiza la llamada al sistema "escribir cadena".

; Se muestra el mensaje informativo en pantalla.
mov ah, 9 ; Se carga el número de la función "escribir cadena" en el registro AH.
mov dx, mensaje2 ; Se carga la dirección del mensaje informativo en el registro DX.
int 21h ; Se realiza la llamada al sistema "escribir cadena".

; Se espera a que el usuario presione cualquier tecla.
mov ah, 1 ; Se carga el número de la función "leer carácter" en el registro AH.
int 21h ; Se realiza la llamada al sistema "leer carácter".

; Se muestra el mensaje de despedida en pantalla.
mov ah, 9 ; Se carga el número de la función "escribir cadena" en el registro AH.
mov dx, mensaje3 ; Se carga la dirección del mensaje de despedida en el registro DX.
int 21h ; Se realiza la llamada al sistema "escribir cadena".

; Se termina el programa.
mov ax, 4C00h ; Se carga el número de la función "terminar programa" en el registro AX.
int 21h ; Se realiza la llamada al sistema "terminar programa".
```

Este programa en lenguaje ensamblador es un ejemplo de programa sencillo que muestra mensajes en pantalla y espera a que el usuario presione cualquier tecla.

El programa comienza definiendo una sección de datos, que contiene las cadenas de texto que se mostrarán en pantalla.

Luego, se define una sección de código, que contiene las instrucciones que se ejecutarán.

La primera instrucción del código es `mov ax, datos`, que carga la dirección de la sección de datos en el registro AX.

La siguiente instrucción es `mov ds, ax`, que copia el contenido del registro AX en el registro DS.

Esto es necesario para que el programa pueda acceder a los datos de la sección de datos.

A continuación, el programa muestra el mensaje de bienvenida en pantalla utilizando la función "escribir cadena".

La función "escribir cadena" se llama utilizando la instrucción `int 21h`.

El argumento de la función es la dirección de la cadena que se quiere mostrar.

El programa luego muestra el mensaje informativo en pantalla de la misma manera.

Después, el programa espera a que el usuario presione cualquier tecla utilizando la función "leer carácter".

La función "leer carácter" se llama utilizando la instrucción `int 21h`.

El argumento de la función es la dirección de una variable donde se almacenará el carácter leído.

El programa finalmente muestra el mensaje de despedida en pantalla de la misma manera que los mensajes anteriores.

Por último, el programa termina utilizando la función "terminar programa".

La función "terminar programa" se llama utilizando la instrucción `int 21h`.

El argumento de la función es el código de retorno del programa.