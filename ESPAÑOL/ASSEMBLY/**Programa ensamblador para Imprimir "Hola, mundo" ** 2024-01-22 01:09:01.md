```assembly

; Este es un programa de computadora en lenguaje ensamblador que imprime un mensaje en la pantalla

; Define una sección de datos para almacenar el mensaje
data_section:
    message: db 'Hola, mundo!', 10, 13, '$' ; El mensaje que se imprimirá

; Define una sección de código para ejecutar el programa
code_section:

    ; Inicia el programa
    start:

    ; Carga el registro DX con la dirección del mensaje
    mov dx, offset message

    ; Carga el registro AH con el código de la función de impresión de texto
    mov ah, 9

    ; Llama a la función de impresión de texto
    int 21h

    ; Finaliza el programa
    mov ah, 4Ch
    int 21h


; Este es el final del programa


; Explicación del código:

; La primera línea del código define una sección de datos llamada "data_section". Esta sección se utiliza para almacenar el mensaje que se imprimirá.

; La segunda línea del código define una variable llamada "message" dentro de la sección de datos. Esta variable se utiliza para almacenar el mensaje que se imprimirá.

; La tercera línea del código define una sección de código llamada "code_section". Esta sección se utiliza para ejecutar el programa.

; La cuarta línea del código define una etiqueta llamada "start". Esta etiqueta se utiliza para marcar el inicio del programa.

; La quinta línea del código carga el registro DX con la dirección de la variable "message". Esto se hace para que cuando se llame a la función de impresión de texto, el registro DX contenga la dirección del mensaje que se imprimirá.

; La sexta línea del código carga el registro AH con el código de la función de impresión de texto. El código de la función de impresión de texto es 9.

; La séptima línea del código llama a la función de impresión de texto. Esto se hace para imprimir el mensaje que se almacenó en la variable "message".

; La octava línea del código finaliza el programa. Esto se hace para que cuando el programa haya terminado de ejecutarse, se cierre la ventana de la consola.


```