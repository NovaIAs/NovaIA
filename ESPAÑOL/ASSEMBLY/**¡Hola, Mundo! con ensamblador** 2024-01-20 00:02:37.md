```
.MODEL TINY  ; Especifica que el programa es un modelo pequeño

.STACK 100H  ; Reserva 100H bytes para la pila

.DATA

mensaje: DB '¡Hola, Mundo!'
final: DB 10, '$'  ; Código ASCII para salto de línea y signo de dólar

.CODE

inicio:

    ; Carga la dirección del mensaje en el registro DS
    MOV AX, @DATA
    MOV DS, AX

    ; Muestra el mensaje en la pantalla
    MOV AH, 9
    MOV DX, OFFSET mensaje
    INT 21H

    ; Termina el programa
    MOV AH, 4CH
    INT 21H

FIN
```

Explicación:

* `.MODEL TINY`: Especifica que el programa es un modelo pequeño. Esto significa que el programa se ejecutará en un segmento de memoria único.
* `.STACK 100H`: Reserva 100H bytes para la pila. La pila es un área de memoria que se utiliza para almacenar datos temporales durante la ejecución del programa.
* `.DATA`: Define el segmento de datos. El segmento de datos contiene los datos que se utilizarán en el programa.
* `mensaje`: Define una variable de cadena que contiene el mensaje "¡Hola, Mundo!".
* `final`: Define una variable de cadena que contiene un salto de línea y un signo de dólar. Esto se utiliza para terminar la salida del programa.
* `.CODE`: Define el segmento de código. El segmento de código contiene las instrucciones que se ejecutarán cuando se ejecute el programa.
* `inicio`: Define el punto de entrada del programa. El punto de entrada es la primera instrucción que se ejecuta cuando se ejecuta el programa.
* `MOV AX, @DATA`: Carga la dirección del segmento de datos en el registro AX.
* `MOV DS, AX`: Carga la dirección del segmento de datos en el registro DS. Esto le dice al procesador dónde encontrar los datos que se utilizarán en el programa.
* `MOV AH, 9`: Carga el código de la función `INT 21H` en el registro AH. El código de la función `INT 21H` es el código para mostrar una cadena de caracteres en la pantalla.
* `MOV DX, OFFSET mensaje`: Carga la dirección de la variable `mensaje` en el registro DX. Esto le dice al procesador dónde encontrar la cadena de caracteres que se mostrará en la pantalla.
* `INT 21H`: Llama a la función `INT 21H` para mostrar la cadena de caracteres en la pantalla.
* `MOV AH, 4CH`: Carga el código de la función `INT 21H` en el registro AH. El código de la función `INT 21H` es el código para terminar el programa.
* `INT 21H`: Llama a la función `INT 21H` para terminar el programa.