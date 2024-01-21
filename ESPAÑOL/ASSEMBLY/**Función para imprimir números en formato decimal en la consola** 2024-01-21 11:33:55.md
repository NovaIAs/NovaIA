```assembly
;Cargar el registro AX con el valor 0
mov ax, 0

;Comenzar un bucle que se repita mientras AX sea menor que 10
ciclo:
    cmp ax, 10          ;Comparar el valor de AX con 10
    jge fin_ciclo       ;Si es mayor o igual, saltar al final del bucle

    ;Imprimir el valor de AX en la consola
    mov dl, al         ;Mover el valor de AX al registro DL (almacena el byte de menos peso de AX)
    call imprimir_numero

    ;Incrementar el valor de AX en 1
    inc ax

    ;Saltar al comienzo del bucle
    jmp ciclo

fin_ciclo:

;Imprimir un mensaje de finalización
mov dl, '¡Listo!'     ;Cargar el mensaje en el registro DL
call imprimir_cadena

;Finalizar el programa
mov ax, 4C00h         ;Establecer el código de salida en 0
int 21h                 ;Llamar a la función de salida de DOS

;Subrutina para imprimir un carácter
imprimir_caracter:
    push bx             ;Guardar el valor de BX en la pila
    mov ah, 02h         ;Establecer el código de función para imprimir un carácter
    mov dl, [bx]         ;Cargar el carácter en el registro DL
    int 21h                 ;Llamar a la función de impresión de DOS
    pop bx              ;Restaurar el valor de BX desde la pila
    ret                 ;Regresar a la función que llamó a esta subrutina

;Subrutina para imprimir una cadena de caracteres
imprimir_cadena:
    push bx             ;Guardar el valor de BX en la pila
    mov ah, 09h         ;Establecer el código de función para imprimir una cadena
    mov dx, [bx]         ;Cargar la dirección de la cadena en el registro DX
    int 21h                 ;Llamar a la función de impresión de DOS
    pop bx              ;Restaurar el valor de BX desde la pila
    ret                 ;Regresar a la función que llamó a esta subrutina

;Subrutina para imprimir un número en formato decimal
imprimir_numero:
    push bx             ;Guardar el valor de BX en la pila
    push dx             ;Guardar el valor de DX en la pila
    mov ax, 0            ;Inicializar el acumulador a 0
    mov bx, 1000h        ;Establecer el divisor a 1000h
div_loop:
    div bx              ;Dividir AX por BX
    push dx             ;Guardar el cociente en la pila
    cmp ax, 0            ;¿El resto de la división es 0?
    je fin_div_loop      ;Si es así, salir del bucle
    jmp div_loop       ;Si no, repetir el bucle
fin_div_loop:
    pop dx              ;Restaurar el cociente de la pila
    cmp dx, 0            ;¿El cociente es 0?
    je imprimir_0       ;Si es así, imprimir un 0
    push dx             ;Guardar el cociente en la pila
    mov cx, 0            ;Inicializar el contador a 0
contar_digitos:
    inc cx              ;Incrementar el contador
    mov ax, dx          ;Mover el cociente al acumulador
    div 10              ;Dividir AX por 10
    mov dx, ax          ;Mover el cociente al registro DX
    cmp ax, 0            ;¿El resto de la división es 0?
    jne contar_digitos   ;Si no, repetir el bucle
fin_contar_digitos:
    sub cx, 1           ;Restar 1 del contador para obtener el número de dígitos
    mov si, [bx]         ;Cargar la dirección de la cadena de dígitos en el registro SI
    mov di, 0            ;Inicializar el índice del dígito a 0
imprimir_digitos:
    cmp di, cx          ;¿El índice es igual al número de dígitos?
    je fin_imprimir_digitos  ;Si es así, salir del bucle
    pop dx              ;Restaurar el dígito de la pila
    mov al, dx          ;Mover el dígito al acumulador
    add al, '0'         ;Sumar '0' al dígito para obtener el carácter ASCII
    mov [si+di], al     ;Almacenar el carácter en la cadena de dígitos
    inc di              ;Incrementar el índice del dígito
    jmp imprimir_digitos
fin_imprimir_digitos:
    mov byte [si+di], 0  ;Terminar la cadena de dígitos con un carácter nulo
    call imprimir_cadena  ;Imprimir la cadena de dígitos
    jmp fin_imprimir_numero
imprimir_0:
    mov byte [si], '0'    ;Almacenar un 0 en la cadena de dígitos
    mov byte [si+1], 0    ;Terminar la cadena de dígitos con un carácter nulo
    call imprimir_cadena  ;Imprimir la cadena de dígitos
fin_imprimir_numero:
    pop dx              ;Restaurar el valor de DX desde la pila
    pop bx              ;Restaurar el valor de BX desde la pila
    ret                 ;Regresar a la función que llamó a esta subrutina
```

Explicación:

Este código es una implementación de una función para imprimir números en formato decimal en la consola. La función funciona convirtiendo el número a una cadena de caracteres, y luego imprimiendo la cadena.

La función comienza cargando el registro AX con el valor 0, y luego entra en un bucle que se repite mientras AX sea menor que 10. Dentro del bucle, la función llama a la subrutina imprimir_numero para imprimir el valor de AX en la consola. A continuación, la función incrementa el valor de AX en 1 y salta al comienzo del bucle.

La subrutina imprimir_numero funciona convirtiendo el número en una cadena de caracteres. Primero, la función inicializa el acumulador AX a 0 y el divisor BX a 1000h. A continuación, la función entra en un bucle que divide AX por BX y guarda el cociente en la pila. El bucle se repite hasta que el resto de la división sea 0.

A continuación, la función restaura el cociente de la pila y comprueba si es 0. Si es así, la función imprime un 0. De lo contrario, la función entra en otro bucle que cuenta el número de dígitos en el cociente.

Después de contar el número de dígitos, la función restaura el cociente de la pila y entra en un bucle que imprime los dígitos uno por uno. La función utiliza la subrutina imprimir_caracter para imprimir cada dígito.

Finalmente, la función termina la cadena de dígitos con un carácter nulo y llama a la subrutina imprimir_cadena para imprimir la cadena.