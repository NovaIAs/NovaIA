```assembly
; Este programa calcula el factorial de un número.

; Sección de datos
.data
numero: .word 5 ; El número cuyo factorial queremos calcular
factorial: .word 1 ; El factorial del número

; Sección de código
.text
main:
    ; Almacenamos el número en el registro eax
    mov eax, numero

    ; Inicializamos el factorial en 1
    mov factorial, 1

    ; Calculamos el factorial multiplicando el número por todos los números
    ; menores que él
    loop:
        ; Multiplicamos el factorial por el número
        mul eax

        ; Disminuimos el número en 1
        dec eax

        ; Si el número es mayor que 1, volvemos al principio del bucle
        cmp eax, 1
        jg loop

    ; Devolvemos el factorial en el registro eax
    ret
```