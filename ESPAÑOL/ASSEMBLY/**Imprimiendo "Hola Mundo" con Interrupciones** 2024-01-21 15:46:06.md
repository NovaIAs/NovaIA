```assembly
; Este programa muestra el mensaje "hola mundo!" en pantalla utilizando la interrupción 0x10

; Definimos un segmento de código
code_segment:
    mov ax, 0x1000 ; Establecemos el puntero de código al inicio del segmento
    mov ds, ax     ; Establecemos el registro de segmento de datos al mismo valor
    mov es, ax     ; Establecemos el registro de segmento extra al mismo valor

; Definimos un segmento de datos
data_segment:
    message db "¡Hola mundo!", 13, 10, '$' ; El mensaje que queremos mostrar, seguido de un salto de línea y un carácter nulo
    string_length equ $-message ; Longitud de la cadena

; Definimos la rutina principal del programa
main:
    mov ax, 0x09 ; Función de escritura (imprimir una cadena)
    mov dx, message ; Dirección de la cadena a imprimir
    mov bx, string_length ; Longitud de la cadena
    int 0x10 ; Llamada a la interrupción 0x10 para imprimir la cadena

    mov ax, 0x4c ; Función de salida (terminar el programa)
    int 0x21 ; Llamada a la interrupción 0x21 para terminar el programa

; Definimos la interrupción 0x10 (imprimir una cadena)
isr_10:
    push ax ; Guardamos el registro ax en la pila
    push bx ; Guardamos el registro bx en la pila
    push cx ; Guardamos el registro cx en la pila
    push dx ; Guardamos el registro dx en la pila

    ; Mostramos la cadena
    mov ah, 0x0e ; Función de escritura (imprimir una cadena)
    mov bx, dx ; Dirección de la cadena a imprimir
    mov cx, bx ; Longitud de la cadena
    int 0x10 ; Llamada a la interrupción 0x10 para imprimir la cadena

    ; Restauramos los registros de la pila
    pop dx ; Restauramos el registro dx de la pila
    pop cx ; Restauramos el registro cx de la pila
    pop bx ; Restauramos el registro bx de la pila
    pop ax ; Restauramos el registro ax de la pila

    iret ; Volvemos del procedimiento de interrupción

; Definimos la interrupción 0x21 (terminar el programa)
isr_21:
    ; Terminamos el programa
    mov ah, 0x4c ; Función de salida (terminar el programa)
    int 0x21 ; Llamada a la interrupción 0x21 para terminar el programa

; Definimos el final del programa
end main
```

Este programa es un poco más complejo que el anterior, ya que incluye una rutina de interrupción para imprimir una cadena. La rutina de interrupción se activa cuando se llama a la función de escritura (int 0x10). La rutina de interrupción guarda los registros de la pila, muestra la cadena y restaura los registros de la pila.

El programa también incluye una rutina de interrupción para terminar el programa (int 0x21). Esta rutina de interrupción simplemente llama a la función de salida (ah = 0x4c) para terminar el programa.