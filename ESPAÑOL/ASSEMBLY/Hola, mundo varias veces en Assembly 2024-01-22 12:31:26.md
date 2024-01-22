```assembly
; Un ejemplo complejo de código en Assembly

; Definir los segmentos de datos y código
.data
  ; Tabla de cadenas
  saludo:     .asciiz "Hola, mundo!"
  salto_linea: .ascii "\n"

.code

; Definir la etiqueta de inicio
inicio:

  ; Cargar el registro AX con el número 1
  mov ax, 1

  ; Cargar el registro BX con la dirección de la tabla de cadenas
  lea bx, [saludo]

  ; Realizar un bucle para imprimir cada cadena de la tabla
  bucle:

    ; Llamar a la función de impresión de cadenas
    call imprimir_cadena

    ; Añadir un salto de línea
    call imprimir_salto_linea

    ; Incrementar el registro AX en 1
    inc ax

    ; Comparar el registro AX con el número 5
    cmp ax, 5

    ; Si AX es menor que 5, saltar al principio del bucle
    jl bucle

  ; Salir del programa
  int 0x20

; Función para imprimir una cadena de caracteres
imprimir_cadena:

  ; Cargar el registro DX con la dirección de la cadena
  mov dx, [bx]

  ; Cargar el registro AH con el número 9
  mov ah, 9

  ; Realizar una llamada al sistema para imprimir la cadena
  int 0x21

  ; Retornar a la dirección de llamada
  ret

; Función para imprimir un salto de línea
imprimir_salto_linea:

  ; Cargar el registro DX con la dirección del salto de línea
  mov dx, [salto_linea]

  ; Cargar el registro AH con el número 9
  mov ah, 9

  ; Realizar una llamada al sistema para imprimir el salto de línea
  int 0x21

  ; Retornar a la dirección de llamada
  ret
```

Este código crea una tabla de cadenas, e itera sobre ella imprimiendo cada cadena en una línea independiente. Una vez que se han impreso las cinco cadenas, el programa finaliza.

Explicación del código:

* El segmento de datos contiene la definición de la tabla de cadenas. Cada cadena está terminada con un carácter nulo, que indica el final de la cadena.
* El segmento de código contiene la etiqueta de inicio, que es el punto de entrada del programa.
* La función `imprimir_cadena` recibe como parámetro la dirección de una cadena de caracteres y la imprime en la consola.
* La función `imprimir_salto_linea` imprime un salto de línea en la consola.
* El bucle principal del programa itera sobre la tabla de cadenas, llamando a la función `imprimir_cadena` para imprimir cada cadena y a la función `imprimir_salto_linea` para imprimir un salto de línea después de cada cadena.
* Una vez que se han impreso las cinco cadenas, el programa finaliza llamando a la función `int 0x20`.