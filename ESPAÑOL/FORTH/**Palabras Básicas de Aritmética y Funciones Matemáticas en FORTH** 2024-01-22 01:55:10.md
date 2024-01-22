```forth

\ Definir palabras básicas

: SOMA ; Suma dos números.
: RESTA ; Resta un número del otro.
: MULTIPLICAR ; Multiplica dos números.
: DIVIDIR ; Divide un número por el otro.
: MOD ; Devuelve el resto de la división de dos números.

: PRINT_STACK ; Imprime la pila.

\ Definir palabras más complejas

: FACTORIAL ; Calcula el factorial de un número.

    DUP 0= IF
        1 EXIT
    THEN
    1- LOOP
        SWAP
        *
    REPEAT
    DROP ;

: SUMA_DIGITOS ; Calcula la suma de los dígitos de un número.

    DUP 0= IF
        0 EXIT
    THEN
    MOD 10 + SWAP 10 /
    LOOP
    DROP ;

: POTENCIA ; Calcula la potencia de un número.

    SWAP DUP 0= IF
        1 EXIT
    THEN
    1- LOOP
        SWAP *
    REPEAT
    DROP ;

: BUSCAR_PRIMOS ; Genera una lista de números primos hasta un número determinado.

    DUP 2= IF
        2, EXIT
    THEN

    3 BEGIN
        DUP 3* > WHILE
            DROP
            2+
        REPEAT
    2+
    LOOP

    SWAP 1 ?DO
        I 2 >
        WHILE
            MOD 0= IF
                DROP EXIT
            THEN
        1+
        LOOP
    LOOP
    DROP ;

: MAYOR_COMUN_DIVISOR ; Calcula el máximo común divisor de dos números.

    DUP
    WHILE
        MOD SWAP
    REPEAT
    DROP ;

: MENOR_COMUN_MULTIPLO ; Calcula el mínimo común múltiplo de dos números.

    DUP
    SWAP
    DIVIDIR
    DUP MAYOR_COMUN_DIVISOR * ;

\ Definir palabras de prueba

: PRUEBA_SOMA ; Prueba la palabra SOMA.

    10 7 SOMA .
    20 14 SOMA .

: PRUEBA_RESTA ; Prueba la palabra RESTA.

    15 10 RESTA .
    5 3 RESTA .

: PRUEBA_MULTIPLICAR ; Prueba la palabra MULTIPLICAR.

    15 3 MULTIPLICAR .
    20 20 MULTIPLICAR .

: PRUEBA_DIVIDIR ; Prueba la palabra DIVIDIR.

    12 4 DIVIDIR .
    100 5 DIVIDIR .

: PRUEBA_MOD ; Prueba la palabra MOD.

    17 4 MOD .
    20 3 MOD .

: PRUEBA_PRINT_STACK ; Prueba la palabra PRINT_STACK.

    10 15 20 PRINT_STACK .
    "Hola, mundo!" PRINT_STACK .

: PRUEBA_FACTORIAL ; Prueba la palabra FACTORIAL.

    5 FACTORIAL .
    10 FACTORIAL .

: PRUEBA_SUMA_DIGITOS ; Prueba la palabra SUMA_DIGITOS.

    123 SUMA_DIGITOS .
    987 SUMA_DIGITOS .

: PRUEBA_POTENCIA ; Prueba la palabra POTENCIA.

    2 10 POTENCIA .
    3 5 POTENCIA .

: PRUEBA_BUSCAR_PRIMOS ; Prueba la palabra BUSCAR_PRIMOS.

    100 BUSCAR_PRIMOS .

: PRUEBA_MAYOR_COMUN_DIVISOR ; Prueba la palabra MAYOR_COMUN_DIVISOR.

    12 18 MAYOR_COMUN_DIVISOR .
    21 14 MAYOR_COMUN_DIVISOR .

: PRUEBA_MENOR_COMUN_MULTIPLO ; Prueba la palabra MENOR_COMUN_MULTIPLO.

    12 18 MENOR_COMUN_MULTIPLO .
    21 14 MENOR_COMUN_MULTIPLO .

\ Ejecutar pruebas

PRUEBA_SOMA
CRLF
PRUEBA_RESTA
CRLF
PRUEBA_MULTIPLICAR
CRLF
PRUEBA_DIVIDIR
CRLF
PRUEBA_MOD
CRLF
PRUEBA_PRINT_STACK
CRLF
PRUEBA_FACTORIAL
CRLF
PRUEBA_SUMA_DIGITOS
CRLF
PRUEBA_POTENCIA
CRLF
PRUEBA_BUSCAR_PRIMOS
CRLF
PRUEBA_MAYOR_COMUN_DIVISOR
CRLF
PRUEBA_MENOR_COMUN_MULTIPLO

```

Explicación:

- Definimos palabras básicas como SOMA, RESTA, MULTIPLICAR, DIVIDIR y MOD.
- Definimos palabras más complejas como FACTORIAL, SUMA_DIGITOS, POTENCIA, BUSCAR_PRIMOS, MAYOR_COMUN_DIVISOR y MENOR_COMUN_MULTIPLO.
- Definimos palabras de prueba para cada palabra para demostrar su funcionamiento.
- Finalmente, ejecutamos las pruebas para mostrar los resultados.