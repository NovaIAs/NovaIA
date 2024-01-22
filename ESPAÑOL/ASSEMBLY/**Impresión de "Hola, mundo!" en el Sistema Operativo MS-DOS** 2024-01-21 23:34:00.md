```assembly
ORG 0x0100

; Definición de las áreas de memoria
SEGMENTO DE DATOS SEGMENT
    MENSAJE DB "Hola, mundo!", 10, 13, '$'
SEGMENTO DE DATOS ENDS

; Definición del segmento de código
SEGMENTO DE CÓDIGO SEGMENT
    ASSUME DS:[SEGMENTO DE DATOS]

; Definición de la etiqueta principal
INICIO:

; Impresión del mensaje en la pantalla
    MOV AH, 0x09
    MOV DX, OFFSET MENSAJE
    INT 0x21

; Fin del programa
    MOV AX, 0x4C00
    INT 0x21

SEGMENTO DE CÓDIGO ENDS

; Fin del programa
END INICIO
```

Explicación del código:

* **ORG 0x0100:** Esta línea indica al ensamblador que el código debe comenzar en la dirección de memoria 0x0100.
* **SEGMENTO DE DATOS SEGMENT:** Esta línea define el inicio del segmento de datos.
* **MENSAJE DB "Hola, mundo!", 10, 13, '$':** Esta línea define una variable llamada "MENSAJE" que contiene el texto "Hola, mundo!". Los caracteres 10 y 13 representan el salto de línea y el retorno de carro, respectivamente. El carácter '$' indica el final de la cadena de texto.
* **SEGMENTO DE DATOS ENDS:** Esta línea define el final del segmento de datos.
* **SEGMENTO DE CÓDIGO SEGMENT:** Esta línea define el inicio del segmento de código.
* **ASSUME DS:[SEGMENTO DE DATOS]:** Esta línea indica al ensamblador que el registro DS debe apuntar al segmento de datos.
* **INICIO:** Esta etiqueta marca el inicio del programa.
* **MOV AH, 0x09:** Esta instrucción mueve el valor 0x09 en el registro AH. Este valor indica a la función de impresión de MS-DOS que se debe imprimir una cadena de caracteres.
* **MOV DX, OFFSET MENSAJE:** Esta instrucción mueve el desplazamiento de la variable "MENSAJE" en el registro DX. El desplazamiento es la dirección de la variable en el segmento de datos.
* **INT 0x21:** Esta instrucción invoca la función de impresión de MS-DOS.
* **MOV AX, 0x4C00:** Esta instrucción mueve el valor 0x4C00 en el registro AX. Este valor indica a la función de salida de MS-DOS que se debe terminar el programa.
* **INT 0x21:** Esta instrucción invoca la función de salida de MS-DOS.
* **SEGMENTO DE CÓDIGO ENDS:** Esta línea define el final del segmento de código.
* **END INICIO:** Esta línea indica al ensamblador que el programa termina en la etiqueta "INICIO".