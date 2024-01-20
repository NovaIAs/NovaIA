```
; Este código en ASSEMBLY es un programa que calcula el factorial de un número usando una función recursiva.

.data
; Mensaje de bienvenida
mensaje_bienvenida: .asciz "Bienvenido al programa de cálculo factorial."

; Etiqueta para la entrada del usuario
entrada: .asciz "Ingresa un número no negativo: "

; Etiqueta para mostrar el resultado
resultado: .asciz "El factorial del número es: "

; Etiqueta para el error de entrada
error_entrada: .asciz "Error: el número ingresado no es válido."

; Variables para almacenar el número y el factorial
numero: .int 0
factorial: .int 0

.code
; Punto de entrada del programa
main:
    ; Imprimir el mensaje de bienvenida
    pusha
    push mensaje_bienvenida
    call imprimir_cadena
    add esp, 4
    popa

    ; Imprimir la etiqueta para la entrada del usuario
    pusha
    push entrada
    call imprimir_cadena
    add esp, 4
    popa

    ; Leer el número ingresado por el usuario
    call leer_numero

    ; Comprobar si el número ingresado es válido
    cmp numero, 0
    jl error_entrada  ; Saltar a la etiqueta de error si el número es negativo

    ; Si el número es válido, calcular su factorial
    call calcular_factorial

    ; Imprimir el resultado
    pusha
    push factorial
    call imprimir_numero
    add esp, 4
    popa

    pusha
    push resultado
    call imprimir_cadena
    add esp, 4
    popa

    ; Finalizar el programa
    ret

; Función para imprimir una cadena de caracteres
imprimir_cadena:
    pusha
    push eax
    push ebx
    push ecx
    push edx

    mov ecx, dword [esp+16]  ; Dirección de la cadena

    loop:
        mov al, byte [ecx]
        cmp al, 0
        je fin_imprimir  ; Salir del bucle si el byte es cero (fin de la cadena)
        call imprimir_caracter
        inc ecx
        jmp loop

    fin_imprimir:
    popa
    pop edx
    pop ecx
    pop ebx
    pop eax
    ret

; Función para imprimir un número
imprimir_numero:
    pusha
    push eax
    push ebx
    push ecx
    push edx

    mov ecx, dword [esp+16]  ; Dirección del número

    call convertir_a_cadena
    mov eax, dword [esp+16]  ; Dirección de la cadena convertida

    call imprimir_cadena

    popa
    pop edx
    pop ecx
    pop ebx
    pop eax
    ret

; Función para convertir un número a una cadena de caracteres
convertir_a_cadena:
    pusha
    push eax
    push ebx
    push ecx
    push edx

    mov ecx, 10  ; Base de conversión (10 para números decimales)
    mov ebx, esp+8  ; Dirección del búfer de salida
    mov [ebx], 0  ; Inicializar el búfer de salida con cero

    mov eax, dword [esp+20]  ; Número a convertir

    loop:
        mov edx, 0  ; Dividendo
        mov edx, eax
        div ecx  ; eax = eax / ecx, edx = eax % ecx

        add al, '0'  ; Convertir el dígito a un carácter ASCII
        mov [ebx], al  ; Almacenar el dígito en el búfer de salida
        inc ebx  ; Incrementar el puntero del búfer de salida

        mov eax, edx  ; eax = edx
        cmp eax, 0
        jne loop  ; Si eax no es cero, continuar el bucle

    popa
    pop edx
    pop ecx
    pop ebx
    pop eax
    ret

; Función para leer un número ingresado por el usuario
leer_numero:
    pusha
    push eax
    push ebx
    push ecx
    push edx

    mov ecx, entrada  ; Dirección de la etiqueta de entrada
    call imprimir_cadena

    call obtener_linea
    mov eax, dword [esp+16]  ; Dirección de la línea ingresada

    call convertir_a_entero
    mov numero, dword [esp+16]  ; Almacenar el número convertido en la variable numero

    popa
    pop edx
    pop ecx
    pop ebx
    pop eax
    ret

; Función para obtener una línea de texto ingresada por el usuario
obtener_linea:
    pusha
    push eax
    push ebx
    push ecx
    push edx

    mov eax, 0  ; System call number (0 = read)
    mov ebx, 0  ; File descriptor (0 = stdin)
    mov ecx, dword [esp+20]  ; Dirección del búfer de entrada
    mov edx, 1024  ; Tamaño del búfer de entrada

    int 0x80  ; Realizar la llamada al sistema

    popa
    pop edx
    pop ecx
    pop ebx
    pop eax
    ret

; Función para convertir una cadena de caracteres a un entero
convertir_a_entero:
    pusha
    push eax
    push ebx
    push ecx
    push edx

    mov ecx, dword [esp+20]  ; Dirección de la cadena
    mov eax, 0  ; Resultado inicializado a cero

    loop:
        mov al, byte [ecx]
        cmp al, 0
        je fin_convertir  ; Salir del bucle si el byte es cero (fin de la cadena)

        sub al, '0'  ; Convertir el carácter numérico a un dígito
        mul ecx
        add eax, al
        inc ecx
        jmp loop

    fin_convertir:
    mov dword [esp+16], eax  ; Almacenar el resultado convertido en la dirección especificada por el llamante
    popa
    pop edx
    pop ecx
    pop ebx
    pop eax
    ret

; Función para calcular el factorial de un número usando recursividad
calcular_factorial:
    pusha
    push eax
    push ebx
    push ecx
    push edx

    cmp numero, 1
    je fin_recursivo  ; Si el número es 1, devolver 1

    mov eax, numero  ; Multiplicar el número por el factorial del número anterior
    dec numero
    call calcular_factorial
    mul eax

    fin_recursivo:
    mov factorial, eax  ; Almacenar el resultado en la variable factorial
    popa
    pop edx
    pop ecx
    pop ebx
    pop eax
    ret

; Función para imprimir un carácter
imprimir_caracter:
    pusha
    push eax
    push ebx
    push ecx
    push edx

    mov al, byte [esp+16]  ; Carácter a imprimir

    mov eax, 4  ; Código del sistema (4 = write)
    mov ebx, 1  ; Descriptor de archivo (1 = stdout)
    mov ecx, al  ; Carácter a imprimir
    mov edx, 1  ; Número de bytes a escribir

    int 0x80  ; Realizar la llamada al sistema

    popa
    pop edx
    pop ecx
    pop ebx
    pop eax
    ret

; Etiqueta de error para cuando el usuario ingresa un número negativo
error_entrada:
    pusha
    push eax
    push ebx
    push ecx
    push edx

    mov ecx, error_entrada  ; Dirección de la etiqueta de error
    call imprimir_cadena

    popa
    pop edx
    pop ecx
    pop ebx
    pop eax
    ret
```

Explicación del código:

* La sección `.data` contiene las cadenas de caracteres que se utilizarán en el programa.
* La sección `.code` contiene las instrucciones del programa.
* La función `main` es el punto de entrada del programa.
* La función `imprimir_cadena` imprime una cadena de caracteres en la consola.
* La función `imprimir_numero` imprime un número en la consola.
* La función `convertir_a_cadena` convierte un número a una cadena de caracteres.
* La función `leer_numero` lee un número ingresado por el usuario.
* La función `obtener_linea` obtiene una línea de texto ingresada por el usuario.
* La función `convertir_a_entero` convierte una cadena de caracteres a un entero.
* La función `calcular_factorial` calcula el factorial de un número usando recursividad.
* La función `imprimir_caracter` imprime un carácter en la consola.
* La etiqueta `error_entrada` se utiliza para mostrar un mensaje de error si el usuario ingresa un número negativo.