```assembly
; Este código en Assembly es un programa simple que calcula el factorial de un número ingresado por el usuario.

; Se definen las secciones del programa
.data
; Aquí se definen las variables y los datos que utilizaremos
numero: .word 0 ; Variable para almacenar el número ingresado por el usuario
factorial: .word 1 ; Variable para almacenar el factorial del número
mensaje_numero: .asciz "Ingrese un número entero: "
mensaje_factorial: .asciz "El factorial del número es: "

.text
; Aquí se define la sección de código del programa
.global main ; Se declara la función main como la función de entrada del programa
main:
; Se muestran mensajes en la consola
mov eax, 4 ; Código de sistema para escribir en la consola
mov ebx, 1 ; Descriptor de archivo para la consola
mov ecx, mensaje_numero ; Dirección del mensaje a mostrar
mov edx, largo_mensaje_numero ; Longitud del mensaje
int 80h ; Llamada al sistema para escribir en la consola

; Se lee el número ingresado por el usuario
mov eax, 3 ; Código de sistema para leer desde la consola
mov ebx, 0 ; Descriptor de archivo para la consola
mov ecx, numero ; Dirección de la variable donde se almacenará el número
mov edx, 4 ; Número máximo de caracteres a leer
int 80h ; Llamada al sistema para leer desde la consola

; Se calcula el factorial del número
mov eax, 1 ; Inicializar el factorial a 1
mov ebx, numero ; Cargar el número en el registro ebx
bucle_factorial:
cmp ebx, 1 ; Comparar el valor de ebx con 1
jle fin_bucle_factorial ; Saltar al final del bucle si ebx es menor o igual que 1
mul ebx ; Multiplicar el factorial por ebx
sub ebx, 1 ; Decrementar el valor de ebx
jmp bucle_factorial ; Saltar al inicio del bucle

fin_bucle_factorial:
; Se muestran los resultados en la consola
mov eax, 4 ; Código de sistema para escribir en la consola
mov ebx, 1 ; Descriptor de archivo para la consola
mov ecx, mensaje_factorial ; Dirección del mensaje a mostrar
mov edx, largo_mensaje_factorial ; Longitud del mensaje
int 80h ; Llamada al sistema para escribir en la consola

mov eax, 4 ; Código de sistema para escribir en la consola
mov ebx, 1 ; Descriptor de archivo para la consola
mov ecx, factorial ; Dirección de la variable donde se almacena el factorial
mov edx, 4 ; Número máximo de caracteres a mostrar
int 80h ; Llamada al sistema para escribir en la consola

; Se termina el programa
mov eax, 1 ; Código de sistema para salir del programa
mov ebx, 0 ; Código de retorno del programa
int 80h ; Llamada al sistema para salir del programa

; Se definen las constantes y los datos
largo_mensaje_numero: .equ $-mensaje_numero ; Longitud del mensaje para el número
largo_mensaje_factorial: .equ $-mensaje_factorial ; Longitud del mensaje para el factorial
```

Este código en Assembly es un programa simple que calcula el factorial de un número ingresado por el usuario.

El código está dividido en dos secciones:

1. La sección de datos: Aquí se definen las variables y los datos que utilizaremos.
2. La sección de código: Aquí se define el código del programa.

En la sección de datos, definimos las siguientes variables y datos:

* `numero`: Una variable para almacenar el número ingresado por el usuario.
* `factorial`: Una variable para almacenar el factorial del número.
* `mensaje_numero`: Un mensaje que se mostrará al usuario pidiéndole que ingrese un número.
* `mensaje_factorial`: Un mensaje que se mostrará al usuario mostrando el factorial del número.

En la sección de código, definimos la función `main`, que es la función de entrada del programa.

En la función `main`, primero mostramos los mensajes en la consola utilizando la llamada al sistema `int 80h`. Luego, leemos el número ingresado por el usuario utilizando la llamada al sistema `int 80h`.

A continuación, calculamos el factorial del número utilizando un bucle. En el bucle, multiplicamos el factorial por el número actual y decrementamos el número.

Después de calcular el factorial, lo mostramos en la consola utilizando la llamada al sistema `int 80h`.

Finalmente, terminamos el programa utilizando la llamada al sistema `int 80h`.

Este código es un ejemplo de un programa simple en Assembly que calcula el factorial de un número ingresado por el usuario.