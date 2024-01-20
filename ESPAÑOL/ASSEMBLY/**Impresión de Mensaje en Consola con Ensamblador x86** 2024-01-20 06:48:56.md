```assembly
.data
  mensaje: .asciiz "Hola, mundo!"
  longitud_mensaje: .int 13

.text
  .global main
main:
  ; Definir registros de segmento y apilador inicial
  mov ax, SEG data
  mov ds, ax
  mov ax, SEG stack
  mov ss, ax
  mov sp, _stack_top

  ; Cargar el mensaje en el registro de memoria
  lea bx, [mensaje]
  mov cx, [longitud_mensaje]

  ; Imprimir el mensaje en la consola
  mov ah, 4
  mov dx, bx
  int 21h

  ; Salir del programa
  mov ax, 4c00h
  int 21h

.stack_top:
  .space 256
```

Este código en lenguaje ensamblador x86 para DOS realiza lo siguiente:

1. Define el área de datos y de texto en la memoria.
2. Carga el mensaje "Hola, mundo!" y su longitud en registros de memoria.
3. Imprime el mensaje en la consola utilizando la función DOS 4.
4. Finaliza el programa llamando a la función DOS 4c00h.

El código utiliza las siguientes técnicas de ensamblaje:

* Uso de registros de segmento para definir el área de datos y de texto.
* Uso de la instrucción LEA para cargar la dirección de una variable en un registro de memoria.
* Uso de las instrucciones MOV y CX para cargar el mensaje y su longitud en los registros de memoria.
* Uso de la instrucción INT 21h para llamar a las funciones DOS.
* Uso de la instrucción JMP para saltar a la dirección de la función DOS 4c00h.

Este código es un ejemplo sencillo de un programa en lenguaje ensamblador x86 que imprime un mensaje en la consola. Es un buen punto de partida para aprender a programar en lenguaje ensamblador.