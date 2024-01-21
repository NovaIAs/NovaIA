```assembly
; Código en Assembly para calcular el factorial de un número entero positivo

; Sección de datos
.data
numero: .int 5           ; Variable para almacenar el número del que se quiere calcular el factorial

; Sección de código
.text
.global factorial        ; Hacernos disponible a otras partes del programa

factorial:
    mov eax, [numero]     ; Mover el número a la variable de trabajo eax
    mov ebx, 1             ; Inicializar el valor del factorial a 1
    cmp eax, 1             ; Comprobar si el número es 1
    je fin_factorial        ; Si es 1, ir directamente al final de la función
    dec eax                 ; Decrementar el número para el siguiente ciclo
    mov ecx, eax             ; Copiar el número a ecx para mantener el valor original
    loop inicio_ciclo       ; Ciclo para calcular el factorial

inicio_ciclo:
    mul ebx                 ; Multiplicar el factorial por el número
    dec eax                 ; Decrementar el número para el siguiente ciclo
    cmp eax, 1             ; Comprobar si el número es 1
    jne inicio_ciclo        ; Si no es 1, repetir el ciclo

fin_factorial:
    ret                     ; Devolver el resultado del factorial

.end
```

Explicación:

* Sección de datos:

    * `numero: .int 5`: Declara una variable llamada `numero` con un valor inicial de 5. Esta variable se utiliza para almacenar el número del que se quiere calcular el factorial.

* Sección de código:

    * `factorial:`: Etiqueta que marca el inicio de la función factorial.

    * `mov eax, [numero]`: Mueve el valor de la variable `numero` a la variable de trabajo `eax`.

    * `mov ebx, 1`: Inicializa el valor del factorial a 1.

    * `cmp eax, 1`: Comprueba si el número es 1.

    * `je fin_factorial`: Si el número es 1, salta a la etiqueta `fin_factorial`.

    * `dec eax`: Decrementa el número en 1 para el siguiente ciclo.

    * `mov ecx, eax`: Copia el valor del número a la variable `ecx` para mantener el valor original.

    * `loop inicio_ciclo`: Ciclo que se ejecuta mientras el valor de `eax` sea mayor que 1.

    * `inicio_ciclo:`: Etiqueta que marca el inicio del ciclo.

    * `mul ebx`: Multiplica el valor del factorial por el valor del número.

    * `dec eax`: Decrementa el número en 1 para el siguiente ciclo.

    * `cmp eax, 1`: Comprueba si el número es 1.

    * `jne inicio_ciclo`: Si el número no es 1, salta a la etiqueta `inicio_ciclo`.

    * `fin_factorial:`: Etiqueta que marca el final de la función factorial.

    * `ret`: Devuelve el resultado del factorial.

    * `.end`: Fin de la función factorial.