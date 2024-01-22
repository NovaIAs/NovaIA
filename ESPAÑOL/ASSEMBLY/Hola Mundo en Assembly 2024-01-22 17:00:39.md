```assembly
.386
.model flat,stdcall
.stack 4096

.data
mensaje_hola BYTE "Hola mundo!", 13, 10, '$'

.code
inicio:
    mov eax, 4 ; Función write
    mov ebx, mensaje_hola ; Dirección del mensaje
    mov ecx, $ ; Longitud del mensaje
    int 21h ; Llamar a la interrupción 21h

    mov eax, 1 ; Función exit
    int 21h ; Llamar a la interrupción 21h

end inicio
```

Explicación:

* El código empieza con la directiva `.386`, que indica que el código está destinado a un procesador 80386 o superior.
* La directiva `.model flat,stdcall` indica que el código utilizará el modelo de memoria plano y la convención de llamada stdcall.
* La directiva `.stack 4096` reserva 4096 bytes para la pila.
* La sección `.data` contiene los datos del programa, en este caso, el mensaje "Hola mundo!".
* La sección `.code` contiene el código del programa.
* La etiqueta `inicio` marca el punto de entrada del programa.
* La instrucción `mov eax, 4` carga el valor 4 en el registro eax. Este valor corresponde a la función `write`.
* La instrucción `mov ebx, mensaje_hola` carga la dirección del mensaje "Hola mundo!" en el registro ebx.
* La instrucción `mov ecx, $` carga la longitud del mensaje "Hola mundo!" en el registro ecx.
* La instrucción `int 21h` llama a la interrupción 21h, que es la interrupción responsable de la salida de datos por la consola.
* La instrucción `mov eax, 1` carga el valor 1 en el registro eax. Este valor corresponde a la función `exit`.
* La instrucción `int 21h` llama a la interrupción 21h, que es la interrupción responsable de terminar el programa.
* La etiqueta `end` marca el final del programa.