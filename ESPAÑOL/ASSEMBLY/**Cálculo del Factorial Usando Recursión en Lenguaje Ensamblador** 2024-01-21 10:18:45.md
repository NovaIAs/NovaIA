```assembly
; Este código implementa una función en lenguaje ensamblador que calcula el factorial de un número usando recursión.

; Segment: datos
.data
numero: .word 0 ; Variable para almacenar el número cuyo factorial se quiere calcular
resultado: .word 1 ; Variable para almacenar el resultado del factorial

; Segment: código
.code
inicio:
    ; Obtener el número del usuario
    MOV AX, 3 ; Solicitar al usuario que introduzca un número
    INT 21H
    MOV numero, AX ; Almacenar el número en la variable "numero"

    ; Comprobar si el número es negativo
    CMP numero, 0 ; Comparar el número con 0
    JL negativo ; Si el número es negativo, ir a la etiqueta "negativo"

    ; Calcular el factorial usando recursión
    CALL factorial ; Llamar a la función factorial
    MOV resultado, AX ; Almacenar el resultado del factorial en la variable "resultado"

    ; Mostrar el resultado en pantalla
    MOV AX, 4 ; Solicitar al sistema que escriba una cadena en pantalla
    MOV DS, AX
    LEA DX, mensaje ; Dirección de la cadena a mostrar
    INT 21H

    ; Salir del programa
    MOV AX, 0 ; Solicitar al sistema que finalice el programa
    INT 21H

; Procedimiento: factorial
; Calcula el factorial de un número usando recursión.
; Input: AX - El número cuyo factorial se quiere calcular
; Output: AX - El factorial del número
factorial:
    ; Comprobar si el número es 0
    CMP AX, 0 ; Comparar el número con 0
    JE fin ; Si el número es 0, ir a la etiqueta "fin"

    ; Calcular el factorial multiplicando el número por el factorial del número anterior
    DEC AX ; Restar 1 al número
    CALL factorial ; Llamar a la función factorial para calcular el factorial del número anterior
    MUL AX ; Multiplicar el número por el factorial del número anterior
    RET ; Volver a la función que llamó a esta función

; Etiqueta: negativo
; Se llega a esta etiqueta si el número introducido por el usuario es negativo.
negativo:
    ; Mostrar un mensaje de error en pantalla
    MOV AX, 4 ; Solicitar al sistema que escriba una cadena en pantalla
    MOV DS, AX
    LEA DX, mensaje_error ; Dirección de la cadena de error a mostrar
    INT 21H

    ; Salir del programa
    MOV AX, 0 ; Solicitar al sistema que finalice el programa
    INT 21H

; Mensaje: mensaje
; Se utiliza para mostrar el resultado del factorial en pantalla.
mensaje: .asciz "El factorial es: "

; Mensaje de error: mensaje_error
; Se utiliza para mostrar un mensaje de error en pantalla si el número introducido por el usuario es negativo.
mensaje_error: .asciz "El número introducido es negativo. No se puede calcular el factorial."
```

Explicación del código:

* El código comienza definiendo un segmento de datos y un segmento de código.
* En el segmento de datos se definen las variables `numero` y `resultado` para almacenar el número cuyo factorial se quiere calcular y el resultado del factorial, respectivamente.
* En el segmento de código se define la función `inicio`, que es el punto de entrada del programa.
* La función `inicio` primero obtiene el número del usuario y lo almacena en la variable `numero`.
* Luego, comprueba si el número es negativo. Si el número es negativo, se muestra un mensaje de error en pantalla y se sale del programa.
* Si el número es positivo, se llama a la función `factorial` para calcular el factorial del número.
* El resultado del factorial se almacena en la variable `resultado`.
* Finalmente, se muestra el resultado del factorial en pantalla y se sale del programa.
* La función `factorial` es una función recursiva que calcula el factorial de un número.
* La función `factorial` primero comprueba si el número es 0. Si el número es 0, se devuelve 1, que es el factorial de 0.
* Si el número es mayor que 0, se multiplica el número por el factorial del número anterior y se devuelve el resultado.
* El código utiliza las siguientes llamadas al sistema:
    * `INT 21H, AX = 3` para obtener el número del usuario.
    * `INT 21H, AX = 4` para mostrar una cadena en pantalla.
    * `INT 21H, AX = 0` para salir del programa.