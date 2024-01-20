```assembly
; Este programa en ASSEMBLY es una simulación de un cajero automático.

; Definimos las siguientes variables:
; - saldo: saldo actual de la cuenta
; - importe: importe a retirar o depositar
; - opcion: opción seleccionada por el usuario (1: retirar, 2: depositar, 3: consultar saldo, 4: salir)

; Segmento de datos
.data
saldo: .word 1000 ; Saldo inicial de la cuenta
importe: .word 0 ; Importe a retirar o depositar
opcion: .byte 0 ; Opción seleccionada por el usuario

; Segmento de código
.text
; Función principal
main:
    ; Bucle principal del programa
    loop:
        ; Mostramos el menú de opciones
        mov eax, 4 ; Función de escritura
        mov ebx, 1 ; Salida estándar
        mov ecx, mensaje_menu ; Dirección del mensaje del menú
        mov edx, largo_mensaje_menu ; Longitud del mensaje del menú
        int 80h ; Llamada al sistema

        ; Leemos la opción seleccionada por el usuario
        mov eax, 3 ; Función de lectura
        mov ebx, 0 ; Entrada estándar
        mov ecx, direccion_opcion ; Dirección de la variable opcion
        mov edx, 1 ; Tamaño de la variable opcion (1 byte)
        int 80h ; Llamada al sistema

        ; Comprobamos la opción seleccionada
        cmp opcion, 1 ; ¿Retirar?
        je retirar ; Si es 1, vamos a la función retirar
        cmp opcion, 2 ; ¿Depositar?
        je depositar ; Si es 2, vamos a la función depositar
        cmp opcion, 3 ; ¿Consultar saldo?
        je consultar_saldo ; Si es 3, vamos a la función consultar_saldo
        cmp opcion, 4 ; ¿Salir?
        je salir ; Si es 4, vamos a la función salir

        ; Si ninguna de las opciones anteriores es correcta, mostramos un mensaje de error
        mov eax, 4 ; Función de escritura
        mov ebx, 1 ; Salida estándar
        mov ecx, mensaje_error ; Dirección del mensaje de error
        mov edx, largo_mensaje_error ; Longitud del mensaje de error
        int 80h ; Llamada al sistema

        ; Volvemos al bucle principal
        jmp loop

; Función retirar
retirar:
    ; Leemos el importe a retirar
    mov eax, 3 ; Función de lectura
    mov ebx, 0 ; Entrada estándar
    mov ecx, direccion_importe ; Dirección de la variable importe
    mov edx, 4 ; Tamaño de la variable importe (4 bytes)
    int 80h ; Llamada al sistema

    ; Comprobamos si el importe a retirar es mayor que el saldo
    cmp importe, saldo
    jg error_retirada ; Si el importe es mayor que el saldo, mostramos un mensaje de error
    sub saldo, importe ; Si el importe es menor o igual que el saldo, lo restamos del saldo

    ; Mostramos el mensaje de retirada
    mov eax, 4 ; Función de escritura
    mov ebx, 1 ; Salida estándar
    mov ecx, mensaje_retirada ; Dirección del mensaje de retirada
    mov edx, largo_mensaje_retirada ; Longitud del mensaje de retirada
    int 80h ; Llamada al sistema

    ; Volvemos al bucle principal
    jmp loop

; Función depositar
depositar:
    ; Leemos el importe a depositar
    mov eax, 3 ; Función de lectura
    mov ebx, 0 ; Entrada estándar
    mov ecx, direccion_importe ; Dirección de la variable importe
    mov edx, 4 ; Tamaño de la variable importe (4 bytes)
    int 80h ; Llamada al sistema

    ; Sumamos el importe a depositar al saldo
    add saldo, importe

    ; Mostramos el mensaje de depósito
    mov eax, 4 ; Función de escritura
    mov ebx, 1 ; Salida estándar
    mov ecx, mensaje_deposito ; Dirección del mensaje de depósito
    mov edx, largo_mensaje_deposito ; Longitud del mensaje de depósito
    int 80h ; Llamada al sistema

    ; Volvemos al bucle principal
    jmp loop

; Función consultar saldo
consultar_saldo:
    ; Mostramos el saldo actual de la cuenta
    mov eax, 4 ; Función de escritura
    mov ebx, 1 ; Salida estándar
    mov ecx, mensaje_saldo ; Dirección del mensaje de saldo
    mov edx, largo_mensaje_saldo ; Longitud del mensaje de saldo
    int 80h ; Llamada al sistema

    ; Volvemos al bucle principal
    jmp loop

; Función salir
salir:
    ; Mostramos el mensaje de despedida
    mov eax, 4 ; Función de escritura
    mov ebx, 1 ; Salida estándar
    mov ecx, mensaje_despedida ; Dirección del mensaje de despedida
    mov edx, largo_mensaje_despedida ; Longitud del mensaje de despedida
    int 80h ; Llamada al sistema

    ; Salimos del programa
    mov eax, 1 ; Función de salida
    mov ebx, 0 ; Código de salida (0: éxito)
    int 80h ; Llamada al sistema

; Mensajes de texto
.data
mensaje_menu: .asciiz "Elija una opción:\n1. Retirar\n2. Depositar\n3. Consultar saldo\n4. Salir\n> "
largo_mensaje_menu: .equ $-mensaje_menu
mensaje_error: .asciiz "Error: opción no válida.\n"
largo_mensaje_error: .equ $-mensaje_error
mensaje_retirada: .asciiz "Retirada realizada con éxito.\n"
largo_mensaje_retirada: .equ $-mensaje_retirada
mensaje_deposito: .asciiz "Depósito realizado con éxito.\n"
largo_mensaje_deposito: .equ $-mensaje_deposito
mensaje_saldo: .asciiz "Su saldo actual es: $ "
largo_mensaje_saldo: .equ $-mensaje_saldo
mensaje_despedida: .asciiz "Gracias por usar el cajero automático.\n"
largo_mensaje_despedida: .equ $-mensaje_despedida

; Direcciones de las variables
.data
direccion_opcion: .word opcion
direccion_importe: .word importe