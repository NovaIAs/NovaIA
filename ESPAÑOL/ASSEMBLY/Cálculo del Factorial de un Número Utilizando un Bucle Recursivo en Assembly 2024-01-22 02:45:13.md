```assembly
; Este programa calcula el factorial de un número utilizando un bucle recursivo.

; Declaramos las variables necesarias.

numero: .word 0 ; El número cuyo factorial queremos calcular.
factorial: .word 1 ; El factorial del número.
i: .word 0 ; El índice del bucle recursivo.

; Pedimos al usuario que introduzca el número cuyo factorial quiere calcular.

mov eax, 3 ; Código de llamada a la función `lee_numero`.
mov ebx, numero ; Dirección de la variable `numero`.
int 80h ; Llamada al sistema `lee_numero`.

; Comprobamos si el número es negativo.

cmp numero, 0 ; Compara el valor de `numero` con 0.
jl negativo ; Si `numero` es menor que 0, salta a la etiqueta `negativo`.

; Calculamos el factorial del número utilizando un bucle recursivo.

negativo:
cmp numero, 0 ; Comprueba si `numero` es 0.
je fin ; Si `numero` es 0, salta a la etiqueta `fin`.

mov eax, numero ; Carga el valor de `numero` en el registro `eax`.
sub eax, 1 ; Resta 1 a `eax`.
mov numero, eax ; Almacena el nuevo valor de `eax` en `numero`.

call factorial ; Llama a la función `factorial` de forma recursiva.

; Multiplicamos el factorial del número por el número.

mov eax, factorial ; Carga el valor de `factorial` en el registro `eax`.
mov ebx, numero ; Carga el valor de `numero` en el registro `ebx`.
mul ebx ; Multiplica `eax` por `ebx`.
mov factorial, eax ; Almacena el nuevo valor de `eax` en `factorial`.

; Incrementamos el índice del bucle recursivo.

mov eax, i ; Carga el valor de `i` en el registro `eax`.
add eax, 1 ; Suma 1 a `eax`.
mov i, eax ; Almacena el nuevo valor de `eax` en `i`.

; Saltamos al principio del bucle recursivo.

jmp negativo

; Etiqueta que marca el final del programa.

fin:

; Imprimimos el factorial del número.

mov eax, 4 ; Código de llamada a la función `imprime_numero`.
mov ebx, factorial ; Dirección de la variable `factorial`.
int 80h ; Llamada al sistema `imprime_numero`.

; Terminamos el programa.

mov eax, 1 ; Código de llamada a la función `termina_programa`.
int 80h ; Llamada al sistema `termina_programa`.

; Función que lee un número del usuario.

lee_numero:

; Pedimos al usuario que introduzca un número.

mov eax, 4 ; Código de llamada a la función `lee_cadena`.
mov ebx, mensaje ; Dirección del mensaje a mostrar.
int 80h ; Llamada al sistema `lee_cadena`.

; Convertimos la cadena introducida por el usuario a un número.

mov eax, 5 ; Código de llamada a la función `convierte_cadena_a_numero`.
mov ebx, cadena ; Dirección de la cadena a convertir.
int 80h ; Llamada al sistema `convierte_cadena_a_numero`.

; Devolvemos el número convertido.

ret

; Función que calcula el factorial de un número.

factorial:

; Comprobamos si el número es 0.

cmp numero, 0 ; Compara el valor de `numero` con 0.
je fin_factorial ; Si `numero` es 0, salta a la etiqueta `fin_factorial`.

; Multiplicamos el factorial del número por el número.

mov eax, factorial ; Carga el valor de `factorial` en el registro `eax`.
mov ebx, numero ; Carga el valor de `numero` en el registro `ebx`.
mul ebx ; Multiplica `eax` por `ebx`.
mov factorial, eax ; Almacena el nuevo valor de `eax` en `factorial`.

; Decrementamos el número en 1.

mov eax, numero ; Carga el valor de `numero` en el registro `eax`.
sub eax, 1 ; Resta 1 a `eax`.
mov numero, eax ; Almacena el nuevo valor de `eax` en `numero`.

; Llamamos a la función `factorial` de forma recursiva.

call factorial

; Etiqueta que marca el final de la función `factorial`.

fin_factorial:

; Devolvemos el factorial del número.

ret

; Función que imprime un número en la consola.

imprime_numero:

; Convertimos el número a una cadena.

mov eax, 6 ; Código de llamada a la función `convierte_numero_a_cadena`.
mov ebx, numero ; Dirección del número a convertir.
mov ecx, cadena ; Dirección de la cadena donde se almacenará el número convertido.
int 80h ; Llamada al sistema `convierte_numero_a_cadena`.

; Imprimimos la cadena en la consola.

mov eax, 4 ; Código de llamada a la función `imprime_cadena`.
mov ebx, cadena ; Dirección de la cadena a imprimir.
int 80h ; Llamada al sistema `imprime_cadena`.

; Devolvemos el control al programa principal.

ret

; Función que termina el programa.

termina_programa:

; Terminamos el programa.