```assembly
; Este código en ensamblador crea una estructura de datos compleja y realiza operaciones matemáticas con ella.

; Definir el segmento de datos
datos segmento
    ; Crear una matriz bidimensional de 10 filas y 10 columnas de enteros de 32 bits
    matriz: times 10 dup(times 10 dup(dword 0))

    ; Crear un vector de 10 enteros de 32 bits
    vector: times 10 dup(dword 0)
datos extremos

; Definir el segmento de código
código segmento comience

    ; Procesador de segmentos
    mov ax, datos
    mov ds, ax

    ; Inicializar la matriz y el vector
    mov esi, offset matriz
    mov edi, offset vector
    xor eax, eax
    mov ecx, 100
inicio_bucle1:
    mov [esi], eax
    add esi, 4
    inc eax
    loop inicio_bucle1

    mov esi, offset vector
    xor eax, eax
    inicio_bucle2:
    mov [esi], eax
    add esi, 4
    inc eax
    loop inicio_bucle2

    ; Mostrar la matriz y el vector
    mov esi, offset matriz
    call mostrar_matriz
    mov esi, offset vector
    call mostrar_vector

    ; Realizar operaciones matemáticas con la matriz y el vector
    mov esi, offset matriz
    mov edi, offset vector
    xor eax, eax
    mov ecx, 10
inicio_bucle3:
    mov eax, [esi]
    add eax, [edi]
    mov [esi], eax
    add esi, 4
    add edi, 4
    loop inicio_bucle3

    ; Mostrar la matriz y el vector después de las operaciones
    mov esi, offset matriz
    call mostrar_matriz
    mov esi, offset vector
    call mostrar_vector

    ; Terminar el programa
    mov eax, 4C00h
    int 21h

mostrar_matriz procedimiento
    mov esi, offset matriz
    mov ecx, 10
inicio_bucle4:
    mov eax, [esi]
    call mostrar_número
    add esi, 4
    loop inicio_bucle4
    ret

mostrar_vector procedimiento
    mov esi, offset vector
    mov ecx, 10
inicio_bucle5:
    mov eax, [esi]
    call mostrar_número
    add esi, 4
    loop inicio_bucle5
    ret

mostrar_número procedimiento
    push ecx
    push edx
    mov edx, 10
    xor eax, eax
    div edx
    add al, 30h
    mov edx, al
    push edx

    xor eax, eax
    div edx
    add al, 30h
    mov edx, al
    push edx

    xor eax, eax
    div edx
    add al, 30h
    mov ah, al
    mov al, [esp]
    mov edx, [esp + 4]
    mov ecx, [esp + 8]
    pop edx
    pop ecx
    mov ah, 2
    int 21h
    ret

código extremos
fin
```

Este código en ensamblador realiza las siguientes tareas:

1. Define dos segmentos, datos y código, y asigna el segmento de datos a la memoria.
2. Inicializa una matriz bidimensional de 10 filas y 10 columnas de enteros de 32 bits y un vector de 10 enteros de 32 bits, estableciendo todos los valores en 0.
3. Muestra la matriz y el vector.
4. Realiza operaciones matemáticas con la matriz y el vector, sumando cada elemento de la matriz con el elemento correspondiente del vector.
5. Muestra de nuevo la matriz y el vector.
6. Finaliza el programa.

El código también incluye tres procedimientos: `mostrar_matriz`, `mostrar_vector` y `mostrar_número`. Estos procedimientos se utilizan para mostrar la matriz, el vector y los números individuales, respectivamente.

El procedimiento `mostrar_matriz` recorre la matriz fila por fila y llama al procedimiento `mostrar_número` para mostrar cada elemento.

El procedimiento `mostrar_vector` recorre el vector y llama al procedimiento `mostrar_número` para mostrar cada elemento.

El procedimiento `mostrar_número` convierte un número en una cadena de caracteres y luego llama a la función `int 21h` para mostrar la cadena de caracteres en la pantalla.