```assembly
; Título del programa: "Calculadora de expresiones aritméticas"

; Definición de los registros que se utilizarán
%register eax, ebx, ecx, edx, esi, edi

; Definición de las etiquetas que se utilizarán
inicio:
    mov eax, 0                ; Inicializar el acumulador a 0
    call leer_expresion        ; Leer la expresión aritmética
    call evaluar_expresion    ; Evaluar la expresión aritmética
    call imprimir_resultado    ; Imprimir el resultado de la evaluación
    jmp fin                  ; Terminar el programa

leer_expresion:
    ; Almacenar la dirección de la variable "expresion" en el registro edi
    mov edi, expresion
    call LeerCadena           ; Leer la cadena de texto de la consola

evaluar_expresion:
    ; Inicializar el registro esi al principio de la cadena de texto
    mov esi, expresion
    ; Mientras el carácter actual no sea el carácter de fin de cadena, realizar el bucle
    cmp byte [esi], 0
    jne evaluar_termino
    ; Retornar el valor del acumulador
    ret

evaluar_termino:
    ; Saltar los espacios en blanco
    jmp evaluar_signo
evaluar_signo:
    ; Leer el carácter actual de la cadena de texto
    mov al, [esi]
    ; Si el carácter actual es un signo más, sumar el siguiente término al acumulador
    cmp al, '+'
    je evaluar_termino_positivo
    ; Si el carácter actual es un signo menos, restar el siguiente término del acumulador
    cmp al, '-'
    je evaluar_termino_negativo
    ; Si el carácter actual no es un signo, devolver el acumulador
    ret

evaluar_termino_positivo:
    ; Pasar al siguiente carácter de la cadena de texto
    inc esi
    ; Evaluar el siguiente término
    call evaluar_termino
    ; Sumar el valor del siguiente término al acumulador
    add eax, eax
    ; Retornar el valor del acumulador
    ret

evaluar_termino_negativo:
    ; Pasar al siguiente carácter de la cadena de texto
    inc esi
    ; Evaluar el siguiente término
    call evaluar_termino
    ; Restar el valor del siguiente término del acumulador
    sub eax, eax
    ; Retornar el valor del acumulador
    ret

imprimir_resultado:
    ; Almacenar la dirección de la variable "resultado" en el registro edi
    mov edi, resultado
    ; Almacenar el valor del acumulador en la variable "resultado"
    mov [edi], eax
    ; Imprimir la variable "resultado" en la consola
    call ImprimirCadena

fin:
    ; Terminar el programa
    ret

leer_cadena:
    ; Almacenar la dirección de la variable "cadena" en el registro edi
    mov edi, cadena
    ; Leer la cadena de texto de la consola
    call LeerCadena
    ; Retornar a la dirección de retorno
    ret

imprimir_cadena:
    ; Almacenar la dirección de la variable "cadena" en el registro edi
    mov edi, cadena
    ; Imprimir la cadena de texto en la consola
    call ImprimirCadena
    ; Retornar a la dirección de retorno
    ret

; Definición de las variables globales
expresion db 256 dup(?)      ; Almacena la expresión aritmética
resultado dw ?                 ; Almacena el resultado de la evaluación
cadena db 256 dup(?)          ; Almacena la cadena de texto que se desea imprimir

; Definición de las llamadas al sistema
LeerCadena proc               ; Leer una cadena de texto de la consola
ImprimirCadena proc           ; Imprimir una cadena de texto en la consola
```