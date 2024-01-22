```assembly
; Definir segmento de datos
SEGMENTO DE DATOS
    ; Definir variables
    _contador DB 0               ; Contador de repeticiones
    _mensaje DB "Hola Mundo!"    ; Mensaje a mostrar
    _repetir DB 5               ; Número de repeticiones

; Definir segmento de código
SEGMENTO DE CÓDIGO

    ; Procedimiento principal
    PROC main

        ; Inicializar contador
        MOV AX, 0
        MOV [_contador], AX

        ; Bucle de repetición
        MOV CX, [_repetir]
    L1:
        ; Mostrar mensaje
        MOV DX, OFFSET _mensaje
        CALL mostrar_mensaje

        ; Incrementar contador
        INC [_contador]

        ; Comprobar si se ha llegado al número de repeticiones
        CMP [_contador], CX
        JNE L1

        ; Salir del programa
        MOV AX, 4C00H
        INT 21H

    ; Procedimiento para mostrar un mensaje
    PROC mostrar_mensaje

        ; Guardar los registros
        PUSH AX
        PUSH DX

        ; Imprimir mensaje
        MOV AH, 9
        MOV DL, AL
        INT 21H

        ; Restaurar los registros
        POP DX
        POP AX

        RET

    ENDS

; Terminar el segmento de código
ENDS

```

Este código en lenguaje Assembly crea un bucle que muestra el mensaje "Hola Mundo!" un número determinado de veces.

El código comienza definiendo el segmento de datos, que contiene las variables que se utilizarán en el programa.

Luego, se define el segmento de código, que contiene el código que se ejecutará.

El procedimiento principal, `main`, llama al procedimiento `mostrar_mensaje` para mostrar el mensaje "Hola Mundo!" un número determinado de veces.

El procedimiento `mostrar_mensaje` utiliza la función `INT 21H` para imprimir el mensaje en la pantalla.

El código finaliza saliendo del programa utilizando la función `INT 21H`.

El código es complejo y diferente porque utiliza un bucle para mostrar el mensaje un número determinado de veces. También utiliza el procedimiento `mostrar_mensaje` para imprimir el mensaje en la pantalla. El código es raro porque no es común utilizar un bucle para mostrar un mensaje un número determinado de veces en lenguaje Assembly.