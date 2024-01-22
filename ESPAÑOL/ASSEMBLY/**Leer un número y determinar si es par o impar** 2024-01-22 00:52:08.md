```assembly
; Este programa en ASSEMBLY realiza las siguientes operaciones:
; 1. Lee un número entero del usuario.
; 2. Comprueba si el número es par o impar.
; 3. Imprime el número y si es par o impar.

; Definir el segmento de datos
.data
numero: .word 0             ; Variable para almacenar el número ingresado por el usuario
mensaje_par: .ascii "El número es par\n"      ; Mensaje para indicar que el número es par
mensaje_impar: .ascii "El número es impar\n"  ; Mensaje para indicar que el número es impar

; Definir el segmento de código
.code
main:                    ; Punto de entrada del programa

    ; Leer el número del usuario
    mov eax, 3             ; Función de lectura de entero
    mov ecx, numero        ; Dirección de la variable numero
    mov edx, 1             ; Tamaño del tipo de dato entero (4 bytes)
    syscall

    ; Comprobar si el número es par o impar
    mov eax, numero        ; Mover el número a eax
    and eax, 1             ; Operación AND con 1 para comprobar el bit menos significativo
    cmp eax, 0             ; Comparar eax con 0
    je impar                ; Si el resultado es igual a 0, saltar a la etiqueta impar

    ; El número es par
    mov eax, 4             ; Función de escritura de cadena
    mov ecx, mensaje_par  ; Dirección del mensaje de par
    mov edx, 13            ; Tamaño del mensaje de par (incluyendo el carácter de nueva línea)
    syscall

    ; El número es impar
impar:
    mov eax, 4             ; Función de escritura de cadena
    mov ecx, mensaje_impar ; Dirección del mensaje de impar
    mov edx, 14            ; Tamaño del mensaje de impar (incluyendo el carácter de nueva línea)
    syscall

    ; Finalizar el programa
    mov eax, 1             ; Función de salida del programa
    mov ebx, 0             ; Código de salida (0 para éxito)
    syscall
```

**Explicación del código:**

1. **Segmento de datos**:

   * `numero:`: Variable definida para almacenar el número ingresado por el usuario. Es una palabra (`word`) de 2 bytes.
   * `mensaje_par:` y `mensaje_impar:`: Mensajes definidos para indicar si el número es par o impar. Son cadenas de caracteres.

2. **Segmento de código**:

   * `main:`: Punto de entrada del programa.

   * **Lectura del número**:

     * `mov eax, 3`: Se carga el valor `3` en el registro `eax`. Esto indica que queremos utilizar la función de lectura de entero.
     * `mov ecx, numero`: Se carga la dirección de la variable `numero` en el registro `ecx`. Esto indica dónde se debe almacenar el número leído.
     * `mov edx, 1`: Se carga el valor `1` en el registro `edx`. Esto indica el tamaño del tipo de dato entero (4 bytes).
     * `syscall`: Se realiza la llamada al sistema para leer el número del usuario.

   * **Comprobación de si el número es par o impar**:

     * `mov eax, numero`: Se mueve el número leído a `eax`.
     * `and eax, 1`: Se realiza una operación AND entre `eax` y `1`. Esto comprueba el bit menos significativo de `eax`.
     * `cmp eax, 0`: Se compara `eax` con `0`.
     * `je impar`: Si el resultado de la comparación es igual a `0`, se salta a la etiqueta `impar`. Esto significa que el número es impar.

   * **Impresión del número y del mensaje correspondiente**:

     * Si el número es par, se imprime el mensaje `"El número es par\n"`.
     * Si el número es impar, se imprime el mensaje `"El número es impar\n"`.

   * **Finalización del programa**:

     * `mov eax, 1`: Se carga el valor `1` en `eax`. Esto indica que queremos utilizar la función de salida del programa.
     * `mov ebx, 0`: Se carga el valor `0` en `ebx`. Esto indica que el programa ha finalizado con éxito.
     * `syscall`: Se realiza la llamada al sistema para finalizar el programa.