```assembly
.MODEL SMALL
.STACK 100H

.DATA
MSG1 DB 13,10,'Ingrese un número entero:',13,10,'$'
MSG2 DB 13,10,'El número ingresado es: ',13,10,'$'
MSG3 DB 13,10,'El número ingresado es negativo',13,10,'$'
BUFFER DB 80 DUP(?)
NUMERO DW ?

.CODE
INICIO:
    MOV AX,@DATA
    MOV DS,AX

    MOV AH,9
    MOV DX,OFFSET MSG1
    INT 21H

    MOV AH,1
    MOV DX,OFFSET BUFFER
    INT 21H

    MOV AH,0
    MOV AL,BUFFER[0]
    SUB AL,30H
    MOV NUMERO,AX

    CMP NUMERO,0
    JGE POSITIVO

    MOV AH,9
    MOV DX,OFFSET MSG3
    INT 21H
    JMP FIN

POSITIVO:
    MOV AH,9
    MOV DX,OFFSET MSG2
    INT 21H

    MOV AH,2
    MOV DL,NUMERO
    INT 21H

FIN:
    MOV AX,4C00H
    INT 21H

END INICIO
```

Explicación:

* El programa comienza con la directiva `.MODEL SMALL`, que especifica que se va a generar un programa de memoria pequeña.
* La directiva `.STACK 100H` define el tamaño de la pila en 100 bytes.
* La sección `.DATA` contiene las siguientes variables:
    * `MSG1`: Un mensaje que se mostrará en la pantalla solicitando al usuario que ingrese un número entero.
    * `MSG2`: Un mensaje que se mostrará en la pantalla junto con el número ingresado por el usuario.
    * `MSG3`: Un mensaje que se mostrará en la pantalla si el número ingresado por el usuario es negativo.
    * `BUFFER`: Un búfer de 80 bytes para almacenar el número ingresado por el usuario.
    * `NUMERO`: Una variable de 16 bits para almacenar el número ingresado por el usuario.
* La sección `.CODE` contiene el siguiente código:
    * `INICIO:`: La etiqueta de inicio del programa.
    * `MOV AX,@DATA`: Carga el segmento de datos en el registro AX.
    * `MOV DS,AX`: Carga el segmento de datos en el registro DS.
    * `MOV AH,9`: Carga la función `INT 21H` en el registro AH.
    * `MOV DX,OFFSET MSG1`: Carga la dirección del mensaje `MSG1` en el registro DX.
    * `INT 21H`: Ejecuta la función `INT 21H` para mostrar el mensaje `MSG1` en la pantalla.
    * `MOV AH,1`: Carga la función `INT 21H` en el registro AH.
    * `MOV DX,OFFSET BUFFER`: Carga la dirección del búfer `BUFFER` en el registro DX.
    * `INT 21H`: Ejecuta la función `INT 21H` para leer una línea de texto del teclado y almacenarla en el búfer `BUFFER`.
    * `MOV AH,0`: Carga la función `INT 21H` en el registro AH.
    * `MOV AL,BUFFER[0]`: Carga el primer carácter del búfer `BUFFER` en el registro AL.
    * `SUB AL,30H`: Resta 30 al valor de `AL` para convertirlo en un número entero.
    * `MOV NUMERO,AX`: Carga el número entero en la variable `NUMERO`.
    * `CMP NUMERO,0`: Compara el valor de `NUMERO` con 0.
    * `JGE POSITIVO`: Si `NUMERO` es mayor o igual que 0, salta a la etiqueta `POSITIVO`.
    * `MOV AH,9`: Carga la función `INT 21H` en el registro AH.
    * `MOV DX,OFFSET MSG3`: Carga la dirección del mensaje `MSG3` en el registro DX.
    * `INT 21H`: Ejecuta la función `INT 21H` para mostrar el mensaje `MSG3` en la pantalla.
    * `JMP FIN`: Salta a la etiqueta `FIN`.
    * `POSITIVO:`: La etiqueta para el número positivo.
    * `MOV AH,9`: Carga la función `INT 21H` en el registro AH.
    * `MOV DX,OFFSET MSG2`: Carga la dirección del mensaje `MSG2` en el registro DX.
    * `INT 21H`: Ejecuta la función `INT 21H` para mostrar el mensaje `MSG2` en la pantalla.
    * `MOV AH,2`: Carga la función `INT 21H` en el registro AH.
    * `MOV DL,NUMERO`: Carga el valor de `NUMERO` en el registro DL.
    * `INT 21H`: Ejecuta la función `INT 21H` para mostrar el valor de `NUMERO` en la pantalla.
    * `FIN:`: La etiqueta para el final del programa.
    * `MOV AX,4C00H`: Carga el código de salida en el registro AX.
    * `INT 21H`: Ejecuta la función `INT 21H` para