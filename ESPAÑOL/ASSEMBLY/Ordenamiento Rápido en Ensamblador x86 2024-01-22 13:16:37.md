```assembly
; Este código implementa el algoritmo de ordenamiento rápido en ensamblador x86.

; Declaración de las variables globales.
global _ordenamiento_rapido
global _particionar

; Esta función ordena un arreglo de enteros usando el algoritmo de ordenamiento rápido.
_ordenamiento_rapido:
    push rbp                      ; Guardamos el valor de rbp en la pila.
    mov rbp, rsp                  ; Establecemos rbp como el nuevo puntero de la pila.
    push rdi                      ; Guardamos el valor de rdi en la pila.
    push rsi                      ; Guardamos el valor de rsi en la pila.
    mov rdi, [rbp + 16]           ; Cargamos la dirección del arreglo en rdi.
    mov rsi, [rbp + 24]           ; Cargamos el tamaño del arreglo en rsi.
    call _particionar             ; Llamamos a la función _particionar.
    mov rbx, rax                  ; Guardamos el índice del pivote en rbx.
    cmp rbx, 0                     ; Comparamos rbx con 0.
    jle fin_ordenamiento           ; Si rbx es menor o igual que 0, terminamos el ordenamiento.
    mov rdi, [rbp + 16]           ; Cargamos la dirección del arreglo en rdi.
    mov rsi, rbx                  ; Cargamos el índice del pivote en rsi.
    call _ordenamiento_rapido     ; Llamamos recursivamente a _ordenamiento_rapido.
fin_ordenamiento:
    mov rdi, [rbp + 16]           ; Cargamos la dirección del arreglo en rdi.
    mov rsi, [rbp + 24]           ; Cargamos el tamaño del arreglo en rsi.
    add rsi, rbx                  ; Sumamos el índice del pivote al tamaño del arreglo.
    call _ordenamiento_rapido     ; Llamamos recursivamente a _ordenamiento_rapido.
    pop rsi                      ; Restauramos el valor de rsi de la pila.
    pop rdi                      ; Restauramos el valor de rdi de la pila.
    pop rbp                      ; Restauramos el valor de rbp de la pila.
    ret                           ; Regresamos a la función que nos llamó.

; Esta función particiona un arreglo de enteros y devuelve el índice del pivote.
_particionar:
    push rbp                      ; Guardamos el valor de rbp en la pila.
    mov rbp, rsp                  ; Establecemos rbp como el nuevo puntero de la pila.
    push rdi                      ; Guardamos el valor de rdi en la pila.
    push rsi                      ; Guardamos el valor de rsi en la pila.
    mov rdi, [rbp + 16]           ; Cargamos la dirección del arreglo en rdi.
    mov rsi, [rbp + 24]           ; Cargamos el tamaño del arreglo en rsi.
    mov rax, rsi                  ; Copiamos el tamaño del arreglo en rax.
    dec rax                       ; Restamos 1 a rax.
    mov rbx, rax                  ; Copiamos el tamaño del arreglo menos 1 en rbx.
    mov rdx, [rdi + rbx * 4]      ; Cargamos el último elemento del arreglo en rdx.
    mov rcx, 0                     ; Inicializamos el índice del pivote en 0.
loop_inicio:
    cmp rcx, rsi                  ; Comparamos rcx con rsi.
    jge loop_fin                  ; Si rcx es mayor o igual que rsi, terminamos el bucle.
    mov rax, [rdi + rcx * 4]      ; Cargamos el elemento actual del arreglo en rax.
    cmp rax, rdx                  ; Comparamos rax con rdx.
    jle loop_continuar            ; Si rax es menor o igual que rdx, continuamos con el bucle.
    mov [rdi + rcx * 4], rdx      ; Intercambiamos el elemento actual del arreglo con el pivote.
    mov [rdi + rbx * 4], rax      ; Movemos el pivote a su nueva posición.
    inc rbx                       ; Incrementamos el índice del pivote.
    inc rcx                       ; Incrementamos el índice del elemento actual.
    jmp loop_inicio               ; Saltamos al inicio del bucle.
loop_continuar:
    inc rcx                       ; Incrementamos el índice del elemento actual.
    jmp loop_inicio               ; Saltamos al inicio del bucle.
loop_fin:
    mov [rdi + rbx * 4], rdx      ; Movemos el pivote a su nueva posición.
    mov rax, rbx                  ; Devolvemos el índice del pivote.
    pop rsi                      ; Restauramos el valor de rsi de la pila.
    pop rdi                      ; Restauramos el valor de rdi de la pila.
    pop rbp                      ; Restauramos el valor de rbp de la pila.
    ret                           ; Regresamos a la función que nos llamó.
```

Explicación:

* La función `_ordenamiento_rapido` recibe dos parámetros: el arreglo de enteros a ordenar y el tamaño del arreglo.
* La función `_particionar` recibe dos parámetros: el arreglo de enteros a particionar y el tamaño del arreglo.
* La función `_ordenamiento_rapido` llama recursivamente a sí misma para ordenar el arreglo.
* La función `_particionar` utiliza el algoritmo de particionamiento para dividir el arreglo en dos partes: una parte con los elementos menores que el pivote y otra parte con los elementos mayores que el pivote.
* La función `_particionar` devuelve el índice del pivote.
* La función `_ordenamiento_rapido` utiliza el índice del pivote para dividir el arreglo en dos subarreglos y llama recursivamente a sí misma para ordenar cada subarreglo.