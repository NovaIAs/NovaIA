```assembly
; Este programa calcula el factorial de un número usando recursividad.

; Definir los registros que se van a utilizar.
R0: registro para el número al que se le va a calcular el factorial.
R1: registro para el resultado del factorial.
R2: registro temporal.

; Definir las etiquetas que se van a utilizar.
INICIO: etiqueta para el inicio del programa.
CALCULAR_FACTORIAL: etiqueta para la función que calcula el factorial.
RETORNAR: etiqueta para retornar de la función.

; Código para el inicio del programa.
INICIO:
    ; Cargar el número al que se le va a calcular el factorial en el registro R0.
    MOV R0, 5

    ; Llamar a la función CALCULAR_FACTORIAL para calcular el factorial.
    CALL CALCULAR_FACTORIAL

    ; Almacenar el resultado del factorial en la memoria.
    MOV [RESULTADO], R1

    ; Terminar el programa.
    HLT

; Código para la función CALCULAR_FACTORIAL.
CALCULAR_FACTORIAL:
    ; Comprobar si el número en el registro R0 es 0.
    CMP R0, 0

    ; Si el número es 0, el factorial es 1.
    JEQ RETORNAR

    ; Multiplicar el número en el registro R0 por el factorial del número anterior.
    MOV R2, R0
    DEC R0
    CALL CALCULAR_FACTORIAL
    MUL R1, R2

    ; Retornar el resultado del factorial.
RETORNAR:
    RET

; Definir la dirección de memoria donde se almacenará el resultado del factorial.
RESULTADO: DB 0
```

Explicación:

Este programa calcula el factorial de un número usando recursividad.

El programa comienza en la etiqueta INICIO, donde se carga el número al que se le va a calcular el factorial en el registro R0.

A continuación, se llama a la función CALCULAR_FACTORIAL para calcular el factorial.

La función CALCULAR_FACTORIAL comprueba si el número en el registro R0 es 0. Si el número es 0, el factorial es 1.

Si el número no es 0, se multiplica el número en el registro R0 por el factorial del número anterior. Para ello, se llama recursivamente a la función CALCULAR_FACTORIAL con el número anterior.

El resultado del factorial se almacena en el registro R1.

Finalmente, se retorna de la función CALCULAR_FACTORIAL.

El resultado del factorial se almacena en la memoria en la dirección RESULTADO.

El programa termina en la etiqueta HLT.