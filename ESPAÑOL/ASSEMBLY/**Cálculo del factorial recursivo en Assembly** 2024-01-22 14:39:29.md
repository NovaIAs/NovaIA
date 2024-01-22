```assembly
; Programa para calcular el factorial de un número entero
; utilizando una función recursiva

; Función para calcular el factorial de un número
factorial PROC

    push bp
    mov bp, sp

    push di
    mov di, [bp + 6]

    cmp di, 1
    je _return_1

    dec di
    call factorial

    mul di

_return_1:
    pop di
    pop bp

    ret

; Función principal
main PROC

    push bp
    mov bp, sp

    mov ax, @data
    push ax

    call factorial

    mov ax, [bp + 6]
    call write_number

    mov ax, 0
    pop bp

    ret

; Función para escribir un número en pantalla
write_number PROC

    push bp
    mov bp, sp

    mov ax, [bp + 6]

    cmp ax, 0
    je _return_0

    mov ah, 2
    mov dl, '0'
    add dl, al
    int 21h

    mov ax, ax / 10
    jmp short write_number

_return_0:
    pop bp

    ret

; Sección de datos
segment data
    times 512 db 0 ; Área de almacenamiento temporal
    factorial_result dw ? ; Resultado del factorial
segment ends

; Sección de código
segment code
    mov ax, @data
    mov ds, ax

    mov ax, @main
    call ax

    mov ax, 4c00h
    int 21h

; Direcciones de las funciones
@factorial PROC
@main PROC
@write_number PROC

; Direcciones de los datos
@data segment dw 0
segment ends

; Punto de entrada
start:
    jmp short main
end start
```

Explicación del código:

* La función `factorial` calcula el factorial de un número entero. Utiliza una llamada recursiva para calcular el factorial del número anterior y luego multiplica el resultado por el número original.
* La función `main` llama a la función `factorial` para calcular el factorial del número 5 y luego escribe el resultado en la pantalla.
* La función `write_number` escribe un número en la pantalla. Utiliza una llamada a la interrupción 21h para escribir los dígitos del número uno a la vez.
* La sección de datos contiene un área de almacenamiento temporal y una variable para almacenar el resultado del factorial.
* La sección de código contiene las funciones `factorial`, `main` y `write_number`.
* El punto de entrada del programa es la etiqueta `start`. Cuando se ejecuta el programa, salta a la etiqueta `main` y comienza a ejecutar el código allí.