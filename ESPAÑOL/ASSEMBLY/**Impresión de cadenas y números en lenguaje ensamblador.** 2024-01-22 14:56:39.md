```assembly

.data

/* Tabla de cadenas */
cadena_hola: .asciiz "Hola, mundo!\n"
cadena_longitud: .asciiz "La longitud de la cadena es: %d\n"

/* Variable para almacenar la longitud de la cadena */
longitud_cadena: .int 0

.text

/* Sección de código */

/* Función principal */
inicio:

  /* Llamada a la función 'obtener_longitud_cadena' */
  call obtener_longitud_cadena

  /* Imprime la longitud de la cadena */
  push longitud_cadena
  call imprimir_entero
  pop longitud_cadena

  /* Imprime la cadena "Hola, mundo!" */
  push cadena_hola
  call imprimir_cadena
  pop cadena_hola

  /* Finaliza el programa */
  ret

/* Función para obtener la longitud de una cadena */
obtener_longitud_cadena:

  /* Registros utilizados: eax, ebx */

  /* eax: índice del carácter actual */
  /* ebx: longitud de la cadena */

  /* Inicializa la longitud de la cadena a 0 */
  xor ebx, ebx

  /* Recorre la cadena hasta encontrar el carácter nulo */
  mov eax, 0
bucle_longitud:
    cmp byte [cadena_hola + eax], 0
    je fin_bucle_longitud
    inc ebx
    inc eax
    jmp bucle_longitud
fin_bucle_longitud:

  /* Devuelve la longitud de la cadena */
  ret

/* Función para imprimir un entero */
imprimir_entero:

  /* Registros utilizados: eax, ebx, ecx, edx */

  /* eax: valor del entero a imprimir */
  /* ebx: copia del valor del entero a imprimir */
  /* ecx: dígito actual */
  /* edx: número de dígitos del entero */

  /* Inicializa el número de dígitos a 0 */
  xor edx, edx

  /* Copia el valor del entero a imprimir */
  mov ebx, eax

  /* Calcula el número de dígitos del entero */
  mov ecx, 10
bucle_digitos:
    idiv ecx
    inc edx
    cmp eax, 0
    jne bucle_digitos

  /* Imprime los dígitos del entero */
  mov eax, 10
bucle_imprimir:
    dec edx
    mov ecx, 10
    idiv ecx
    add eax, '0'
    mov byte [ebx + edx], al
    cmp edx, 0
    jne bucle_imprimir

  /* Imprime el resultado */
  push ebx
  call imprimir_cadena
  pop ebx

  /* Finaliza la función */
  ret

/* Función para imprimir una cadena */
imprimir_cadena:

  /* Registros utilizados: eax, ebx, ecx */

  /* eax: dirección de la cadena a imprimir */
  /* ebx: índice del carácter actual */
  /* ecx: carácter actual */

  /* Inicializa el índice del carácter actual a 0 */
  xor ebx, ebx

bucle_cadena:
    /* Obtiene el carácter actual */
    mov ecx, byte [eax + ebx]

    /* Comprueba si el carácter actual es el carácter nulo */
    cmp ecx, 0
    je fin_bucle_cadena

    /* Imprime el carácter actual */
    mov eax, ecx
    call imprimir_caracter

    /* Incrementa el índice del carácter actual */
    inc ebx

    /* Continúa el bucle */
    jmp bucle_cadena
fin_bucle_cadena:

  /* Finaliza la función */
  ret

/* Función para imprimir un carácter */
imprimir_caracter:

  /* Registros utilizados: eax, ebx */

  /* eax: carácter a imprimir */
  /* ebx: descriptor de archivo de salida */

  /* Obtiene el descriptor de archivo de salida */
  mov ebx, 1

  /* Imprime el carácter */
  mov eax, 1
  mov ecx, eax
  mov edx, eax
  int 0x80

  /* Finaliza la función */
  ret

```

Explicación:

El código anterior es un programa en lenguaje ensamblador que imprime la cadena "Hola, mundo!" y la longitud de la cadena en la consola. El programa utiliza varias funciones para realizar esta tarea, incluyendo una función para obtener la longitud de una cadena, una función para imprimir un entero y una función para imprimir una cadena.

El programa comienza ejecutando la función `inicio`, que llama a la función `obtener_longitud_cadena` para obtener la longitud de la cadena "Hola, mundo!". A continuación, imprime la longitud de la cadena y la cadena "Hola, mundo!" utilizando las funciones `imprimir_entero` e `imprimir_cadena`, respectivamente.

La función `obtener_longitud_cadena` recorre la cadena "Hola, mundo!" carácter por carácter hasta encontrar el carácter nulo, que indica el final de la cadena. La función cuenta el número de caracteres en la cadena y devuelve este valor como resultado.

La función `imprimir_entero` convierte el valor del entero a imprimir en una cadena de caracteres y luego llama a la función `imprimir_cadena` para imprimir la cadena de caracteres en la consola.

La función `imprimir_cadena` recorre la cadena de caracteres carácter por carácter hasta encontrar el carácter nulo, que indica el final de la cadena. La función imprime cada carácter de la cadena en la consola.

El programa utiliza el descriptor de archivo de salida 1, que es el descriptor de archivo estándar de salida. Esto significa que el programa imprimirá su salida en la consola.