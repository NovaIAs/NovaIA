```assembly
; Esta es una función en ensamblador que calcula el factorial de un número.

.data
; Definimos una tabla con los factoriales de los números de 0 a 10.
factoriales:    .word 1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880

.text
; Esta es la función factorial.

factorial:
; Recibe el número cuyo factorial queremos calcular en el registro EAX.
; Devuelve el factorial en el registro EAX.

; Comprobamos si el número es negativo.
    cmp   eax, 0
    jl    error_negativo

; Comprobamos si el número está fuera del rango de la tabla.
    cmp   eax, 10
    jg    error_fuera_de_rango

; Cargamos el factorial de la tabla en el registro EAX.
    mov   eax, [factoriales + eax * 4]

; Devolvemos el factorial.
    ret

; Esta es la etiqueta de error para los números negativos.

error_negativo:
    mov   eax, -1
    ret

; Esta es la etiqueta de error para los números fuera del rango de la tabla.

error_fuera_de_rango:
    mov   eax, -2
    ret

; Este es el punto de entrada del programa.

main:
; Pedimos al usuario que introduzca un número.
    mov   eax, 4
    mov   ebx, 1
    mov   ecx, mensaje_entrada
    mov   edx, longitud_mensaje_entrada
    int   0x21

; Leemos el número introducido por el usuario.
    mov   eax, 3
    mov   ebx, 0
    mov   ecx, buffer
    mov   edx, longitud_buffer
    int   0x21

; Convertimos el número introducido por el usuario a un entero.
    mov   eax, atoi
    mov   ebx, buffer
    call  eax

; Llamamos a la función factorial para calcular el factorial del número.
    mov   eax, [eax]
    call  factorial

; Comprobamos si la función factorial devolvió un error.
    cmp   eax, -1
    je    error_negativo
    cmp   eax, -2
    je    error_fuera_de_rango

; Mostramos el resultado en la pantalla.
    mov   eax, 4
    mov   ebx, 1
    mov   ecx, mensaje_resultado
    mov   edx, longitud_mensaje_resultado
    int   0x21

    mov   eax, 4
    mov   ebx, 1
    mov   ecx, buffer
    mov   edx, longitud_buffer
    int   0x21

    mov   eax, 4
    mov   ebx, 1
    mov   ecx, mensaje_factorial
    mov   edx, longitud_mensaje_factorial
    int   0x21

    mov   eax, 4
    mov   ebx, 1
    mov   ecx, buffer
    mov   edx, longitud_buffer
    int   0x21

; Salimos del programa.
    mov   eax, 1
    mov   ebx, 0
    int   0x21

; Estas son las cadenas de texto que se muestran en la pantalla.

mensaje_entrada:      .asciz "Introduzca un número: "
longitud_mensaje_entrada:    .equ $ - mensaje_entrada
buffer:                   .space 10
longitud_buffer:            .equ $ - buffer
mensaje_resultado:       .asciz "El resultado es: "
longitud_mensaje_resultado:  .equ $ - mensaje_resultado
mensaje_factorial:       .asciz "!"
longitud_mensaje_factorial:  .equ $ - mensaje_factorial
atoi:                      .equ 0x8000

```

Este código es complejo y diferente porque implementa una función para calcular el factorial de un número en ensamblador. El código utiliza una tabla para almacenar los factoriales de los números de 0 a 10, y luego utiliza una función para cargar el factorial de la tabla en el registro EAX. El código también incluye comprobaciones para asegurarse de que el número introducido por el usuario es válido, y muestra el resultado en la pantalla.

El código está escrito en ensamblador, que es un lenguaje de programación de bajo nivel que se utiliza para programar directamente el hardware de un ordenador. El ensamblador es un lenguaje muy difícil de aprender y utilizar, pero también es muy potente y eficiente.

Este código es un ejemplo de cómo se puede utilizar el ensamblador para crear programas complejos y eficientes.