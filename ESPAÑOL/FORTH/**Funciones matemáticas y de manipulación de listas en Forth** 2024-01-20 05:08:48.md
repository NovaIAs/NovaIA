```forth
: SUMA ( n1 -- n2 )
    DUP OVER -
    0> IF
        DROP 0
    ELSE
        SWAP 1 +
        SUMA
    THEN ;

: RESTA ( n1 -- n2 )
    DUP OVER -
    0> IF
        DROP 0
    ELSE
        SWAP 1 -
        RESTA
    THEN ;

: MULTIPLICACIÓN ( n1 -- n2 )
    DUP 0 = IF
        0
    ELSE
        DUP 1 -
        SWAP 1+ MULTIPLICACIÓN
        SUMA
    THEN ;

: DIVISIÓN ( n1 -- n2 )
    DUP 0 = IF
        0
    ELSE
        DUP 1 -
        SWAP 1+ DIVISIÓN
        RESTA
    THEN ;

: MAYOR-QUE ( n1 -- )
    DUP OVER -
    IF
        FALSE
    ELSE
        TRUE
    THEN ;

: MENOR-QUE ( n1 -- )
    DUP OVER -
    0> IF
        TRUE
    ELSE
        FALSE
    THEN ;

: LIST-LENGTH ( list -- n )
    DUP 0 = IF
        0
    ELSE
        DUP 1+ LIST-LENGTH
    THEN ;

: LIST-APPEND ( item list -- new-list )
    OVER DUP @ OVER APPEND SWAP LIST-LENGTH 1+ FILL ;

: LIST-POP ( list -- item list )
    DUP @ DUP 1- OVER SWAP ! SWAP LIST-LENGTH 1- FILL ;

: LIST-REVERSE ( list -- new-list )
    0 SWAP LIST-LENGTH SWAP DO
        I 2* SWAP + @
    LOOP DROP ;

: LIST-FIND ( item list -- item-index | -1 )
    0 SWAP LIST-LENGTH SWAP DO
        I 2* SWAP + @ = IF
            I 2*
        ELSE
            -1
        THEN
    LOOP DROP ;

: LIST-SORT ( list -- sorted-list )
    0 SWAP LIST-LENGTH SWAP DO
        I 2* SWAP + @ J 2* SWAP + @ > IF
            SWAP @ J 2* SWAP + ! I 2* SWAP + !
        THEN
    LOOP DROP ;

```

Este código Forth implementa una serie de funciones matemáticas y de manipulación de listas.

* `SUMA`, `RESTA`, `MULTIPLICACIÓN` y `DIVISIÓN` son las funciones matemáticas básicas.
* `MAYOR-QUE` y `MENOR-QUE` son funciones que comparan dos números y devuelven `TRUE` o `FALSE`.
* `LIST-LENGTH` devuelve la longitud de una lista.
* `LIST-APPEND` añade un elemento al final de una lista.
* `LIST-POP` elimina el último elemento de una lista.
* `LIST-REVERSE` invierte el orden de los elementos de una lista.
* `LIST-FIND` busca un elemento en una lista y devuelve su índice o `-1` si no lo encuentra.
* `LIST-SORT` ordena una lista en orden ascendente.

Este código puede usarse para realizar una amplia variedad de tareas, desde cálculos matemáticos simples hasta la gestión de listas complejas.