```assembly

; Variables
var1: equ 10  ; Valor inicial para la variable 1
var2: equ 20  ; Valor inicial para la variable 2

; Inicio del programa
inicio:

    ; Leemos el valor de var1 y lo guardamos en el registro AX
    mov ax, var1

    ; Leemos el valor de var2 y lo guardamos en el registro BX
    mov bx, var2

    ; Sumamos los valores de AX y BX y lo guardamos en el registro CX
    add cx, ax
    add cx, bx

    ; Dividimos el valor de CX por 2 y lo guardamos en el registro DX
    shr dx, 1

    ; Mostramos el resultado de la suma en la pantalla
    mov ah, 2  ; Función para mostrar caracteres en la pantalla
    mov dl, 'S' ; Mensaje a mostrar
    int 21h    ; Realizamos la llamada al sistema

    ; Mostramos el resultado de la división en la pantalla
    mov ah, 2  ; Función para mostrar caracteres en la pantalla
    mov dl, 'D' ; Mensaje a mostrar
    int 21h    ; Realizamos la llamada al sistema

    ; Finalizamos el programa
    mov ah, 4ch  ; Función para salir del programa
    int 21h    ; Realizamos la llamada al sistema

```

**Explicación del código:**

* La primera línea del programa define una variable llamada `var1` y le asigna el valor de 10.
* La segunda línea del programa define una variable llamada `var2` y le asigna el valor de 20.
* La tercera línea del programa lee el valor de la variable `var1` y lo guarda en el registro `AX`.
* La cuarta línea del programa lee el valor de la variable `var2` y lo guarda en el registro `BX`.
* La quinta línea del programa suma los valores de los registros `AX` y `BX` y guarda el resultado en el registro `CX`.
* La sexta línea del programa divide el valor del registro `CX` por 2 y guarda el resultado en el registro `DX`.
* La séptima línea del programa muestra el valor del registro `CX` en la pantalla.
* La octava línea del programa muestra el valor del registro `DX` en la pantalla.
* La novena línea del programa finaliza el programa.

**Nota:** Este código está escrito en lenguaje ensamblador para el procesador 8086.