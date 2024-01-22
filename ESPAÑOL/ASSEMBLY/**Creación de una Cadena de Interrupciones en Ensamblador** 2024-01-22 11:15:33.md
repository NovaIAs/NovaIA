```
.codigo

start: 
  mov ax, 0x0003  ; Numero de interrupciones
  mov bx, 0x0201  ; Numero de la interrupcion
  int 21h        ; Realizar interrupcion 21h
  
  mov ax, 0x0002  ; Numero de interrupciones
  mov bx, 0x0100  ; Numero de la interrupcion
  int 21h        ; Realizar interrupcion 21h
  
  mov ax, 0x0001  ; Numero de interrupciones
  mov bx, 0x0001  ; Numero de la interrupcion
  int 21h        ; Realizar interrupcion 21h
  
  mov ax, 0x0000  ; Número de interrupciones
  mov bx, 0x0000  ; Número de la interrupción
  int 21h        ; Realizar interrupción 21h
```

Este código en lenguaje ensamblador crea una cadena de interrupciones. Cada interrupción tiene un número y un número de interrupción. El código utiliza la instrucción `int` para realizar la interrupción. El número de interrupciones se guarda en el registro `ax` y el número de interrupción se guarda en el registro `bx`. La interrupción 0x0003 es una interrupción del teclado, la interrupción 0x0002 es una interrupción del reloj del sistema y la interrupción 0x0001 es una interrupción del puerto serie. La interrupción 0x0000 es una interrupción de salida del programa.

El código funciona de la siguiente manera:

1. El código primero carga el registro `ax` con el número de interrupciones (0x0003).
2. A continuación, el código carga el registro `bx` con el número de interrupción (0x0201).
3. El código entonces utiliza la instrucción `int` para realizar la interrupción.
4. El código repite estos pasos para las demás interrupciones.
5. El código finalmente carga el registro `ax` con el número de interrupciones (0x0000) y carga el registro `bx` con el número de interrupción (0x0000).
6. El código entonces utiliza la instrucción `int` para realizar la interrupción.

Este código se puede utilizar para crear una variedad de programas diferentes. Por ejemplo, se puede utilizar para crear un programa que muestre el tiempo en la pantalla, un programa que controle el teclado o un programa que se comunique con un puerto serie.