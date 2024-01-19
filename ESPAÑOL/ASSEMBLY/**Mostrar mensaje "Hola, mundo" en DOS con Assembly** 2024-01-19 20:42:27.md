```assembly
org 100h
inicio:
  mov ah, 2
  int 21h

  mov ah, 9
  mov dx, mensaje
  int 21h

  mov ah, 4ch
  int 21h

mensaje:
  db 'Hola, mundo', '$'
```

Este código en assembly realiza las siguientes acciones:

1. `mov ah, 2`: carga el registro `ah` con el valor 2, que es el código de la función `int 21h` de DOS para obtener el carácter que se está pulsando en el teclado.
2. `int 21h`: ejecuta la función `int 21h` con el valor de `ah` que se le ha pasado, obteniendo así el carácter pulsado por el usuario.
3. `mov ah, 9`: carga el registro `ah` con el valor 9, que es el código de la función `int 21h` de DOS para mostrar un mensaje en pantalla.
4. `mov dx, mensaje`: carga el registro `dx` con la dirección de memoria de la variable `mensaje`, que contiene el mensaje que se quiere mostrar en pantalla.
5. `int 21h`: ejecuta la función `int 21h` con el valor de `ah` y `dx` que se le han pasado, mostrando así el mensaje en pantalla.
6. `mov ah, 4ch`: carga el registro `ah` con el valor 4ch, que es el código de la función `int 21h` de DOS para salir del programa.
7. `int 21h`: ejecuta la función `int 21h` con el valor de `ah` que se le ha pasado, saliendo así del programa.

La variable `mensaje` contiene el mensaje que se quiere mostrar en pantalla, que en este caso es `Hola, mundo`.