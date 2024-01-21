```assembly
; Este programa calcula el factorial de un número usando bucles anidados.

; Se declara la sección de datos.
DATA SEGMENT
    mensaje_factorial db "El factorial de %d es %d"
    número_predeterminado db 5
DATA ENDS

; Se declara la sección de código.
CODE SEGMENT
    assume cs:CODE, ds:DATA

; Se inicia el programa.
inicio:
    mov ax, DATA              ; Carga la dirección de la sección de datos en el registro AX.
    mov ds, ax                ; Establece la sección de datos como el segmento de datos por defecto.

; Se muestra un mensaje solicitando al usuario que introduzca un número.
    mov ah, 9               ; Código de la función "Mostrar cadena".
    mov dx, offset mensaje_entrada
    int 21h                 ; Llamada al sistema para mostrar la cadena.

; Se lee el número introducido por el usuario.
    mov ah, 1                ; Código de la función "Leer cadena".
    mov dx, offset número_usuario
    int 21h                 ; Llamada al sistema para leer la cadena.

; Se convierte el número introducido por el usuario en un número entero.
    mov si, 0                ; Índice del primer carácter del número.
    mov al, número_usuario[si]  ; Carga el primer carácter del número.
    cmp al, 0                ; Comprueba si el carácter es el carácter nulo.
    je fin_conversión        ; Si es el carácter nulo, termina la conversión.

    mov ax, 0                ; Inicializa el acumulador a cero.
    repnz scasb              ; Recorre el número de caracteres del número.
    add ax, 48              ; Suma 48 al acumulador para convertir el carácter en un dígito.

; Se calcula el factorial del número.
    mov bx, ax              ; Carga el número en el registro BX.
    mov cx, 1              ; Inicializa el contador de bucle a 1.
    mov ax, 1              ; Inicializa el acumulador a 1.

bucle_factorial:
    mul bx                  ; Multiplica el acumulador por el número.
    inc cx                  ; Incrementa el contador de bucle.
    cmp cx, bx              ; Comprueba si el contador de bucle es igual al número.
    jne bucle_factorial    ; Si no son iguales, salta al inicio del bucle.

; Se muestra el resultado del cálculo del factorial.
    mov dx, offset mensaje_factorial
    mov ah, 9               ; Código de la función "Mostrar cadena".
    int 21h                 ; Llamada al sistema para mostrar la cadena.

    mov ah, 0               ; Código de la función "Salir del programa".
    int 21h                 ; Llamada al sistema para salir del programa.

; Se declara la sección de datos constantes.
CONST SEGMENT
    mensaje_entrada db "Introduce un número: "
    mensaje_fin db 13, 10, "$"  ; Mensaje de fin de línea y símbolo del sistema.
    número_usuario db 10 dup (?)    ; Buffer para el número introducido por el usuario.
CONST ENDS

; Se termina la sección de código.
CODE ENDS

; Se termina el programa.
END inicio
```

Explicación del código:

* La sección de datos contiene los mensajes que se muestran al usuario, así como el buffer para almacenar el número introducido por el usuario.
* La sección de código contiene el programa principal y las funciones que se utilizan en el programa.
* La función `inicio()` es la función principal del programa. Esta función muestra un mensaje solicitando al usuario que introduzca un número, lee el número introducido por el usuario, convierte el número en un número entero, calcula el factorial del número y muestra el resultado del cálculo del factorial.
* La función `convertir_a_entero()` convierte un número introducido por el usuario en un número entero. Esta función recorre el número de caracteres del número y suma 48 al acumulador para convertir cada carácter en un dígito.
* La función `calcular_factorial()` calcula el factorial de un número. Esta función utiliza un bucle para multiplicar el acumulador por el número hasta que el contador de bucle sea igual al número.
* La sección de datos constantes contiene los mensajes que se muestran al usuario y el buffer para almacenar el número introducido por el usuario.