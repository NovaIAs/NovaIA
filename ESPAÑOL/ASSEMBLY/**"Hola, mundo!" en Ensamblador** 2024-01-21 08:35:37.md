```assembly
; Definir los registros que vamos a utilizar
mov ax, @data             ; Puntero al segmento de datos
mov ds, ax                ; Establecer el segmento de datos
mov bx, @codigo           ; Puntero al segmento de código
mov es, bx                ; Establecer el segmento de código

; Definir los datos que vamos a utilizar
@data segment
mensaje db "Hola, mundo!", 10, 13, '$'
@data ends

; Definir el código que vamos a ejecutar
@codigo segment
inicio:
  mov ax, @mensaje         ; Puntero al mensaje
  mov ds, ax                ; Establecer el segmento de datos
  mov dx, offset mensaje    ; Desplazamiento del mensaje
  mov ah, 9                 ; Función de impresión de cadena
  int 21h                   ; Invocar la interrupción 21h
  mov ah, 4ch               ; Función de salida del programa
  int 21h                   ; Invocar la interrupción 21h

@codigo ends
end inicio
```

Explicación:

* La primera línea define los registros que vamos a utilizar. El registro `ax` se utiliza para almacenar punteros a memoria, el registro `ds` se utiliza para establecer el segmento de datos, el registro `bx` se utiliza para almacenar punteros a memoria, y el registro `es` se utiliza para establecer el segmento de código.
* La segunda línea define los datos que vamos a utilizar. El segmento de datos contiene el mensaje "Hola, mundo!".
* La tercera línea define el código que vamos a ejecutar. La primera instrucción mueve el puntero al segmento de datos al registro `ax`. La segunda instrucción establece el segmento de datos. La tercera instrucción mueve el desplazamiento del mensaje al registro `dx`. La cuarta instrucción establece la función de impresión de cadena en el registro `ah`. La quinta instrucción invoca la interrupción 21h, que imprime la cadena en la pantalla. La sexta instrucción establece la función de salida del programa en el registro `ah`. La séptima instrucción invoca la interrupción 21h, que sale del programa.