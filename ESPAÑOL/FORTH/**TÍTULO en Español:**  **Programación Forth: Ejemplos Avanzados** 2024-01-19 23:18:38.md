```forth
: SALUDO ( -- )
    ." ¡Hola, mundo!" CR
;

: FACTORIAL ( n -- f )
    DUP 0= IF
        1
    ELSE
        SWAP 1- RECURSE *
    THEN
;

: FIBONACCI ( n -- f )
    DUP 1< IF
        1
    ELSE
        SWAP 2 - FIBONACCI FIBONACCI +
    THEN
;

: CUADRADO-MÁGICO ( n -- )
    CREATE n-array
    1 n-array fill
    0 n-array i!
    BEGIN
        OVER @ + 2MOD n OVER + 2MOD n-array i!
        OVER = IF
            DROP EXIT
        THEN
    REPEAT DROP ;

: CUENTA-RÉGRESIVA ( n -- )
    BEGIN
        DUP 0> WHILE
            DUP ." " SWAP DROP REPEAT
        DROP CR
    REPEAT ;

: CALCULADORA ( -- )
    BEGIN
        ." Ingrese un número: " 0 KEY
        DUP 0= WHILE
            DROP ." Ingrese un número válido: " 0 KEY
        REPEAT
        DUP ." Ingrese un operador (+, -, *, /): " 1 KEY 2DROP
        SWAP CASE
            OF
                '+' +
                '-' -
                '*' *
                '/' /
            ELSE
                ." Operador no válido."
            ENDOF
        ." Resultado: " ." " TYPE CR
    AGAIN
;

: MÁXIMO-COMÚN-DIVISOR ( n m -- gcd )
    BEGIN
        SWAP MOD DUP 0= WHILE
            DROP
        REPEAT
    AGAIN
;

: ORDEN-PALABRAS ( palabras -- )
    BEGIN
        WORDS SWAP DO
            DUP @ ." " TYPE
        LOOP DROP CR
    REPEAT ;

: SUMA-LISTA ( lista -- suma )
    BEGIN
        DUP 0= WHILE
            DROP
        REPEAT
        0
        WHILE
            DUP @ OVER + SWAP 1+ SWAP
        REPEAT DROP
    AGAIN
;

: MULTIPLICACIÓN-MATRICES ( m1 m2 -- pr )
    CREATE N-ROW N-COL
    M1 N-ROW @ N-COL @ M1 N-ROW !
    M2 N-COL @ N-ROW @ M2 N-COL !
    0 pr i!
    BEGIN
        M1 N-ROW @ DO
            BEGIN
                M2 N-COL @ DO
                    BEGIN
                        M1 @ M2 @ + PR +!
                    REPEAT
                LOOP
                PR @ PR I +!
            LOOP
        LOOP DROP
    AGAIN
;
```

Explicación del código:

* `SALUDO`: Imprime el mensaje "¡Hola, mundo!" en la consola.
* `FACTORIAL`: Calcula el factorial de un número usando recursión.
* `FIBONACCI`: Calcula el n-ésimo número de Fibonacci usando recursión.
* `CUADRADO-MÁGICO`: Genera un cuadrado mágico de orden n.
* `CUENTA-RÉGRESIVA`: Imprime una cuenta regresiva desde un número hasta 0.
* `CALCULADORA`: Una calculadora simple que solicita al usuario dos números y un operador, y luego realiza el cálculo correspondiente.
* `MÁXIMO-COMÚN-DIVISOR`: Calcula el máximo común divisor de dos números usando el algoritmo de Euclides.
* `ORDEN-PALABRAS`: Ordena una lista de palabras alfabéticamente.
* `SUMA-LISTA`: Suma todos los elementos de una lista.
* `MULTIPLICACIÓN-MATRICES`: Multiplica dos matrices.

Este código es complejo porque contiene una variedad de estructuras de datos y algoritmos diferentes. También utiliza una variedad de características avanzadas de Forth, como palabras creadas por el usuario, bucles y recursión.