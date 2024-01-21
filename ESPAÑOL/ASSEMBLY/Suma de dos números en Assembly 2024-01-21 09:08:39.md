```assembly
; Definimos el segmento de datos
SECCION DATOS
  ; Definimos una variable de texto
  MENSAJE_BIENVENIDA DB "Bienvenido a mi programa en Assembly!", 13, 10, '$'
  ; Definimos una variable numérica entera
  NUMERO_1 DW 10
  ; Definimos una variable numérica real
  NUMERO_2 DD 3.14159265

; Definimos el segmento de código
SECCION CODIGO
  ; Definimos el punto de entrada del programa
  INICIO:
    ; Imprimimos el mensaje de bienvenida
    MOV AX, OFFSET MENSAJE_BIENVENIDA
    MOV DS, AX
    CALL IMPRIMIR_CADENA

    ; Sumamos los dos números
    MOV AX, NUMERO_1
    ADD AX, NUMERO_2
    MOV RESULTADO, AX

    ; Imprimimos el resultado de la suma
    MOV AX, RESULTADO
    CALL IMPRIMIR_NUMERO

    ; Terminamos el programa
    MOV AX, 4C00H
    INT 21H

; Definimos la función para imprimir una cadena de texto
IMPRIMIR_CADENA:
    ; Recibe la dirección de la cadena de texto en AX
    PUSH AX

    ; Imprimimos cada carácter de la cadena de texto
    MOV CX, 0
    MOV AL, [SI]
    CMP AL, '$'
    JE FIN_IMPRIMIR_CADENA
    MOV AH, 02H
    INT 21H
    INC CX
    ADD SI, 1
    JMP IMPRIMIR_CADENA

; Definimos la etiqueta para terminar la función de imprimir una cadena de texto
FIN_IMPRIMIR_CADENA:
    POP AX
    RET

; Definimos la función para imprimir un número
IMPRIMIR_NUMERO:
    ; Recibe el número a imprimir en AX
    PUSH AX

    ; Convertimos el número a una cadena de texto
    MOV AH, 2
    MOV DL, 10
    INT 21H
    MOV SI, OFFSET TEMP
    MOV TEMP, DL

    ; Imprimimos la cadena de texto
    MOV CX, 0
    MOV AL, [SI]
    CMP AL, '$'
    JE FIN_IMPRIMIR_NUMERO
    MOV AH, 02H
    INT 21H
    INC CX
    ADD SI, 1
    JMP IMPRIMIR_NUMERO

; Definimos la etiqueta para terminar la función de imprimir un número
FIN_IMPRIMIR_NUMERO:
    POP AX
    RET

; Definimos el búfer temporal para convertir el número a una cadena de texto
TEMP DB 20, '$'
```

Explicación del código:

* El código está dividido en dos segmentos: el segmento de datos y el segmento de código.
* El segmento de datos contiene las variables del programa, mientras que el segmento de código contiene las instrucciones del programa.
* El punto de entrada del programa es la etiqueta INICIO.
* La primera instrucción del programa es MOV AX, OFFSET MENSAJE_BIENVENIDA. Esta instrucción carga la dirección de la variable MENSAJE_BIENVENIDA en el registro AX.
* La siguiente instrucción es MOV DS, AX. Esta instrucción carga la dirección de la variable MENSAJE_BIENVENIDA en el registro DS.
* La instrucción CALL IMPRIMIR_CADENA llama a la función IMPRIMIR_CADENA, que imprime la cadena de texto almacenada en la variable MENSAJE_BIENVENIDA.
* La siguiente instrucción es MOV AX, NUMERO_1. Esta instrucción carga el valor de la variable NUMERO_1 en el registro AX.
* La siguiente instrucción es ADD AX, NUMERO_2. Esta instrucción suma el valor de la variable NUMERO_2 al valor del registro AX.
* La siguiente instrucción es MOV RESULTADO, AX. Esta instrucción almacena el resultado de la suma en la variable RESULTADO.
* La siguiente instrucción es MOV AX, RESULTADO. Esta instrucción carga el valor de la variable RESULTADO en el registro AX.
* La siguiente instrucción es CALL IMPRIMIR_NUMERO. Esta instrucción llama a la función IMPRIMIR_NUMERO, que imprime el valor del registro AX.
* La última instrucción del programa es MOV AX, 4C00H. Esta instrucción carga el código de terminación del programa en el registro AX.
* La siguiente instrucción es INT 21H. Esta instrucción termina el programa.
* La función IMPRIMIR_CADENA imprime una cadena de texto.
* La función IMPRIMIR_NUMERO imprime un número.