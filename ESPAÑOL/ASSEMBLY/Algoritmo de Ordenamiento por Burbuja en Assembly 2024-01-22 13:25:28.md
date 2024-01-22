```assembly
; Nombre del programa: "Algoritmo de Ordenamiento por Burbuja"
; Propósito: Ordenar un arreglo de números enteros en orden ascendente utilizando el algoritmo de ordenamiento por burbuja.

; Segmento de datos
segment data
    ; Definir un arreglo de diez números enteros
    numeros dw 10, 5, 2, 7, 3, 8, 4, 9, 6, 1

; Segmento de código
segment code

; Punto de entrada del programa
start:

    ; Iniciar un bucle que recorrerá el arreglo desde el primer elemento hasta el último
    mov cx, 10                  ; Inicializar el registro CX con el número de elementos del arreglo
outer_loop:
        ; Iniciar un bucle anidado que recorrerá el arreglo desde el segundo elemento hasta el último
        mov si, 2                 ; Inicializar el registro SI con el índice del segundo elemento
    inner_loop:
            ; Comparar el elemento actual con el elemento anterior
            cmp [numeros + si - 2], [numeros + si]
            jl ordenar             ; Saltar a la etiqueta "ordenar" si el elemento actual es menor que el elemento anterior
            inc si                  ; Incrementar el índice del elemento actual
            cmp si, cx              ; Comparar el índice del elemento actual con el número de elementos del arreglo
            jne inner_loop           ; Saltar a la etiqueta "inner_loop" si el índice del elemento actual es menor que el número de elementos del arreglo

    ; Intercambiar los valores del elemento actual y el elemento anterior
ordenar:
        xchg [numeros + si - 2], [numeros + si]
        dec si                  ; Decrementar el índice del elemento actual
        cmp si, 2                 ; Comparar el índice del elemento actual con el índice del segundo elemento
        jge inner_loop           ; Saltar a la etiqueta "inner_loop" si el índice del elemento actual es mayor o igual que el índice del segundo elemento

    ; Decrementar el número de elementos del arreglo
    dec cx
    cmp cx, 0                   ; Comparar el número de elementos del arreglo con 0
    jne outer_loop              ; Saltar a la etiqueta "outer_loop" si el número de elementos del arreglo es mayor que 0

; Fin del programa
    mov ah, 4ch                  ; Código de finalización del programa
    int 21h                     ; Interrupción para finalizar el programa

; Fin del segmento de código
end start

; Fin del ensamblaje
end
```

Explicación del código:

* El segmento de datos define un arreglo de diez números enteros llamado "numeros".
* El segmento de código contiene el punto de entrada del programa, "start".
* El bucle "outer_loop" recorre el arreglo desde el primer elemento hasta el último.
* El bucle "inner_loop" recorre el arreglo desde el segundo elemento hasta el último.
* La comparación "cmp" compara el elemento actual con el elemento anterior.
* La etiqueta "ordenar" intercambia los valores del elemento actual y el elemento anterior.
* El número de elementos del arreglo se decrementa después de cada pasada del bucle "outer_loop".
* El programa termina cuando el número de elementos del arreglo es igual a 0.