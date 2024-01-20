```assembly
.data

; Mensajes
mensaje_bienvenida: .asciiz "Bienvenido al programa en ensamblador!"
mensaje_solicitar_nombre: .asciiz "Ingrese su nombre: "
mensaje_solicitar_edad: .asciiz "Ingrese su edad: "
mensaje_saludo: .asciiz "Hola, %s! Su edad es %d años."
mensaje_despedida: .asciiz "Hasta luego, %s!"

; Variables
nombre: .space 32
edad: .int 0

; Función para imprimir una cadena de caracteres
imprimir_cadena:
    mov rdi, cadena
    call printf

; Función para obtener una cadena de caracteres del usuario
obtener_cadena:
    mov rdi, cadena
    mov rsi, longitud_maxima
    call scanf

; Función para obtener un número entero del usuario
obtener_entero:
    mov rdi, entero
    call scanf

.text

; Función principal
main:
    ; Imprimir el mensaje de bienvenida
    mov rdi, mensaje_bienvenida
    call imprimir_cadena

    ; Solicitar el nombre al usuario
    mov rdi, mensaje_solicitar_nombre
    call imprimir_cadena
    mov rdi, nombre
    mov rsi, 32
    call obtener_cadena

    ; Solicitar la edad al usuario
    mov rdi, mensaje_solicitar_edad
    call imprimir_cadena
    mov rdi, edad
    call obtener_entero

    ; Imprimir el mensaje de saludo
    mov rdi, mensaje_saludo
    mov rsi, nombre
    mov rdx, edad
    call printf

    ; Imprimir el mensaje de despedida
    mov rdi, mensaje_despedida
    mov rsi, nombre
    call printf

    ; Termina el programa
    mov rax, 0
    ret
```

Explicación:

Este programa en ensamblador realiza lo siguiente:

* Imprime un mensaje de bienvenida.
* Solicita el nombre al usuario.
* Solicita la edad al usuario.
* Imprime un mensaje de saludo, incluyendo el nombre y la edad del usuario.
* Imprime un mensaje de despedida.

El programa se divide en las siguientes secciones:

* `.data`: Esta sección contiene los datos del programa, como las cadenas de caracteres y las variables.
* `.text`: Esta sección contiene el código del programa, como las funciones y la función principal.

La función principal (`main`) del programa realiza lo siguiente:

* Imprime el mensaje de bienvenida.
* Solicita el nombre al usuario.
* Solicita la edad al usuario.
* Imprime un mensaje de saludo, incluyendo el nombre y la edad del usuario.
* Imprime un mensaje de despedida.

Las funciones `imprimir_cadena`, `obtener_cadena` y `obtener_entero` se utilizan para imprimir una cadena de caracteres, obtener una cadena de caracteres del usuario y obtener un número entero del usuario, respectivamente.

El programa utiliza las llamadas al sistema `printf` y `scanf` para imprimir y leer datos de la consola.

El programa termina llamando a la llamada al sistema `exit` con el valor de retorno 0.