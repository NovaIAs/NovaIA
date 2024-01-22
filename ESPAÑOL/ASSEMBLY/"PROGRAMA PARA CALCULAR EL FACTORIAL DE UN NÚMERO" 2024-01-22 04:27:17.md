```assembly
; Programa que calcula el factorial de un número

.model small
.stack 100h

.data
; Definición de la variable para almacenar el número a factorizar
numero dw 0

; Definición de la variable para almacenar el resultado del factorial
factorial dw 0

; Definición del mensaje a mostrar en pantalla
mensaje db "Ingrese el número a factorizar: $"

; Definición del mensaje de error a mostrar en pantalla
error db "Error: El número debe ser positivo."

; Definición del mensaje de resultado a mostrar en pantalla
resultado db "El factorial de $ es $."

; Definición de la tabla de símbolos para la función de entrada
entrada_sym dw "entrada"
entrada_len equ $-entrada_sym

; Definición de la tabla de símbolos para la función de salida
salida_sym dw "salida"
salida_len equ $-salida_sym

.code

; Punto de entrada del programa
inicio:

; Mostrar el mensaje para que el usuario ingrese el número a factorizar
mov ax, @data
mov ds, ax

mov di, mensaje
call imprimir_cadena

; Leer el número ingresado por el usuario
call leer_numero

; Comprobar si el número ingresado es positivo
cmp numero, 0
jl error_numero_negativo

; Calcular el factorial del número
call calcular_factorial

; Mostrar el resultado del factorial
mov ax, @data
mov ds, ax

mov si, resultado
call imprimir_cadena

mov al, numero
call imprimir_numero

mov si, factorial
call imprimir_numero

; Terminar el programa
mov ax, 4C00h
int 21h

; Definición de las funciones

; Función para imprimir una cadena de caracteres
imprimir_cadena:

; Obtener la dirección de la cadena de caracteres
push bx
mov bx, [bp+6]

; Obtener la longitud de la cadena de caracteres
mov cx, [bx+entrada_len]

; Imprimir cada carácter de la cadena de caracteres
loop:
mov dl, [bx]
cmp dl, '$'
je fin_imprimir_cadena
int 21h
inc bx
dec cx
jnz loop

fin_imprimir_cadena:
pop bx
ret

; Función para leer un número entero
leer_numero:

; Obtener la dirección de la variable donde se almacenará el número
push bx
mov bx, [bp+6]

; Leer el número ingresado por el usuario
mov ah, 1
int 21h

; Comprobar si el número ingresado es válido
cmp al, '$'
je error_numero_invalido

; Convertir el número ingresado a un valor numérico
mov cx, 0
mov al, [bx]
sub al, '0'
mov [bx], al
loop:
mul al, 10
inc cx
cmp al, [bx]
jge fin_leer_numero
jmp loop

fin_leer_numero:
pop bx
ret

; Función para calcular el factorial de un número
calcular_factorial:

; Obtener la dirección de la variable donde se almacenará el resultado
push bx
mov bx, [bp+6]

; Inicializar el resultado a 1
mov [bx], 1

; Calcular el factorial del número
mov cx, [bx]
loop:
dec cx
jz fin_calcular_factorial
mul cx
jmp loop

fin_calcular_factorial:
pop bx
ret

; Definición de los procedimientos de error

error_numero_negativo:

; Mostrar el mensaje de error
mov ax, @data
mov ds, ax

mov di, error
call imprimir_cadena

; Terminar el programa
mov ax, 4C00h
int 21h

error_numero_invalido:

; Mostrar el mensaje de error
mov ax, @data
mov ds, ax

mov di, error_numero_invalido
call imprimir_cadena

; Terminar el programa
mov ax, 4C00h
int 21h

; Definición de los datos constantes

mensaje db "Ingrese el número a factorizar: $"
error db "Error: El número debe ser positivo."
error_numero_invalido db "Error: El número ingresado es inválido."
resultado db "El factorial de $ es $."

; Definición de las estructuras de datos

entrada_sym dw "entrada"
entrada_len equ $-entrada_sym

salida_sym dw "salida"
salida_len equ $-salida_sym
```

Explicación del código:

* El programa comienza definiendo las variables y constantes que se utilizarán en el programa.
* El punto de entrada del programa es la etiqueta `inicio`.
* La primera instrucción del programa es `mov ax, @data`, que carga la dirección del segmento de datos en el registro `ax`.
* La siguiente instrucción es `mov ds, ax`, que establece el segmento de datos como el segmento de datos actual.
* A continuación, el programa muestra el mensaje `Ingrese el número a factorizar: $` utilizando la función `imprimir_cadena`.
* Luego, el programa llama a la función `leer_numero` para leer el número ingresado por el usuario.
* Después, el programa comprueba si el número ingresado es positivo. Si el número es negativo, el programa muestra el mensaje `Error: El número debe ser positivo.` y termina.
* Si el número es positivo, el programa llama a la función `calcular_factorial` para calcular el factorial del número.
* Finalmente, el programa muestra el resultado del factorial utilizando la función `imprimir_cadena`.