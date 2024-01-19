```assembly
; Define el segmento de datos
.data
  mensaje: .asciiz "Hola, mundo!"
  longitud: .word 12 ; Tamaño del mensaje en bytes

; Define el segmento de código
.text
; Función principal, punto de entrada del programa
main:
  ; Imprime el mensaje en pantalla
  mov eax, 4  ; Número de sistema para "write"
  mov ebx, 1  ; Descriptor de archivo para stdout
  mov ecx, mensaje  ; Dirección del mensaje
  mov edx, longitud  ; Tamaño del mensaje en bytes
  int 0x80  ; Llamada al sistema

  ; Termina el programa
  mov eax, 1 ; Número de sistema para "exit"
  mov ebx, 0 ; Código de salida
  int 0x80  ; Llamada al sistema

; Fin del programa
end main
```

Explicación:

* El segmento de datos contiene la variable `mensaje` que almacena el texto "Hola, mundo!" y la variable `longitud` que almacena el tamaño del mensaje en bytes.
* El segmento de código contiene la función `main` que es el punto de entrada del programa.
* La función `main` utiliza la llamada al sistema `write` para imprimir el mensaje en pantalla. La llamada al sistema `write` recibe cuatro parámetros:

  * `eax`: Número de sistema para "write".
  * `ebx`: Descriptor de archivo para stdout.
  * `ecx`: Dirección del mensaje.
  * `edx`: Tamaño del mensaje en bytes.

* Después de imprimir el mensaje, la función `main` utiliza la llamada al sistema `exit` para terminar el programa. La llamada al sistema `exit` recibe dos parámetros:

  * `eax`: Número de sistema para "exit".
  * `ebx`: Código de salida.