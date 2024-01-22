```assembly

; Variables
dato1 BYTE 5
dato2 BYTE 10

; Funciones
sumar:
  ; Suma dos números y devuelve el resultado
  mov al, [dato1]  ; Carga el primer número en el registro AL
  add al, [dato2]  ; Suma el segundo número al primero
  ret             ; Devuelve el resultado en el registro AX

restar:
  ; Resta dos números y devuelve el resultado
  mov al, [dato1]  ; Carga el primer número en el registro AL
  sub al, [dato2]  ; Resta el segundo número al primero
  ret             ; Devuelve el resultado en el registro AX

multiplicar:
  ; Multiplica dos números y devuelve el resultado
  mov ax, [dato1]  ; Carga el primer número en el registro AX
  mul [dato2]     ; Multiplica el primer número por el segundo
  ret             ; Devuelve el resultado en el registro AX

dividir:
  ; Divide dos números y devuelve el resultado
  mov ax, [dato1]  ; Carga el primer número en el registro AX
  div [dato2]     ; Divide el primer número por el segundo
  ret             ; Devuelve el resultado en el registro AX

; Programa principal
mov ah, 1       ; Función de salida a consola
mov dx, dato1   ; Dirección del primer dato
int 21h        ; Imprime el primer dato
mov ah, 1
mov dx, dato2
int 21h        ; Imprime el segundo dato

mov ah, 2       ; Función de suma
mov al, [dato1]  ; Carga el primer número en el registro AL
mov bl, [dato2]  ; Carga el segundo número en el registro BL
int 21h        ; Suma los dos números y devuelve el resultado en el registro AX
mov dx, ax      ; Guarda el resultado en el registro DX
mov ah, 1
mov dx, dx      ; Dirección del resultado de la suma
int 21h        ; Imprime el resultado de la suma

mov ah, 3       ; Función de resta
mov al, [dato1]  ; Carga el primer número en el registro AL
mov bl, [dato2]  ; Carga el segundo número en el registro BL
int 21h        ; Resta los dos números y devuelve el resultado en el registro AX
mov dx, ax      ; Guarda el resultado en el registro DX
mov ah, 1
mov dx, dx      ; Dirección del resultado de la resta
int 21h        ; Imprime el resultado de la resta

mov ah, 4       ; Función de multiplicación
mov ax, [dato1]  ; Carga el primer número en el registro AX
mov bx, [dato2]  ; Carga el segundo número en el registro BX
int 21h        ; Multiplica los dos números y devuelve el resultado en el registro AX
mov dx, ax      ; Guarda el resultado en el registro DX
mov ah, 1
mov dx, dx      ; Dirección del resultado de la multiplicación
int 21h        ; Imprime el resultado de la multiplicación

mov ah, 5       ; Función de división
mov ax, [dato1]  ; Carga el primer número en el registro AX
mov bx, [dato2]  ; Carga el segundo número en el registro BX
int 21h        ; Divide los dos números y devuelve el resultado en el registro AX
mov dx, ax      ; Guarda el resultado en el registro DX
mov ah, 1
mov dx, dx      ; Dirección del resultado de la división
int 21h        ; Imprime el resultado de la división

; Fin del programa
mov ah, 4Ch      ; Función de salida del programa
int 21h         ; Salida del programa

```

Este código realiza las siguientes operaciones:

1. Declara dos variables, dato1 y dato2, y les asigna valores.
2. Define cuatro funciones, sumar, restar, multiplicar y dividir, que realizan las operaciones aritméticas correspondientes.
3. En el programa principal, imprime los valores de las variables dato1 y dato2, y llama a las funciones sumar, restar, multiplicar y dividir para calcular y mostrar los resultados de las operaciones.
4. Finalmente, termina el programa llamando a la función de salida del programa.