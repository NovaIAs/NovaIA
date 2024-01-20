```assembly
; Área de datos
SEGMENTO DE DATOS
mensaje: DB '¡Hola, mundo!', 13, 10, '$' ; Mensaje para mostrar
longitudMensaje: DW 14 ; Longitud del mensaje

; Área de código
SEGMENTO DE CÓDIGO
inicio:
    ; Carga la dirección del mensaje en el registro DS
    MOV AX, @datos
    MOV DS, AX

    ; Muestra el mensaje en la consola
    MOV AH, 9 ; Función 9: Escribir cadena
    MOV DX, mensaje ; Dirección del mensaje
    INT 21h ; Interrupción 21h: Servicios del sistema operativo

    ; Termina el programa
    MOV AX, 4C00h ; Función 4C00h: Salir del programa
    INT 21h ; Interrupción 21h: Servicios del sistema operativo

SEGMENTO DE DATOS ENDS
SEGMENTO DE CÓDIGO ENDS

; Final del programa
FIN
```

Explicación del código:

* El código comienza con la definición de las áreas de datos y de código. El área de datos contiene el mensaje que se mostrará en la consola, mientras que el área de código contiene las instrucciones que ejecutará el programa.
* La primera instrucción del área de código carga la dirección del área de datos en el registro DS. Esto es necesario para que el programa pueda acceder a las variables definidas en el área de datos.
* La siguiente instrucción muestra el mensaje en la consola. La función 9 del sistema operativo se utiliza para escribir una cadena de caracteres en la consola. La dirección del mensaje se carga en el registro DX y la longitud del mensaje se carga en el registro CX.
* La última instrucción del área de código termina el programa. La función 4C00h del sistema operativo se utiliza para salir del programa.

Este código es un ejemplo sencillo de un programa en lenguaje ensamblador. El programa muestra un mensaje en la consola y luego termina.