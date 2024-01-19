```forth

\DEFINE: Es un definidor, crea nuevas palabras.
\ DOES> Es un decompilador, muestra una palabra en su forma fuente.

: GUARDAR ( array -- )
    CREATE CELDA ;

: OBTENER ( array -- n )
    @ ;

: PRIMERO ( array -- n )
    0 OBTENER ;

: ULTIMO ( array -- n )
    -1 OBTENER ;

: LARGO ( array -- n )
    1+ ;

\ : Palabra que muestra los valores del array.
: MOSTRAR ( array -- )
    DUP LARGO 0 DO
        I OBTENER .SPACE
    LOOP
    .CR ;

: NUEVO-ARRAY ( n -- array )
    1+ ARRAY CREATE ;

\ : Palabra que agrega un nuevo valor al array.
: AGREGAR ( array x -- )
    DUP LARGO @ + SWAP ! ;

: ARRAY-VACIO ( -- array )
    0 NUEVO-ARRAY ;

\ Palabra que invierte los valores del array.
: INVERTIR ( array -- array )
    DUP 0 PRIMERO DUP ULTIMO
    [ SWAP I @ J @ SWAP ! ]
    WHILE
    DROP ;

\ Palabra que ordena los valores del array de menor a mayor.
: ORDENAR ( array -- array )
    DUP 0 PRIMERO DUP ULTIMO
    [
        J 1+ I + 2DROP
        [ OVER I @ J @ > ]
        WHILE
            [ SWAP J @ I @ SWAP ! ]
        REPEAT
    ]
    WHILE
    DROP ;

\ Palabra que busca un valor en el array y devuelve su índice si lo encuentra, o -1 si no lo encuentra.
: BUSCAR ( array x -- n )
    DUP LARGO 0 DO
        I OBTENER = IF
            DROP I EXIT
        THEN
    LOOP
    DROP -1 ;

\ Palabra que elimina un valor del array.
: ELIMINAR ( array x -- array )
    DUP BUSCAR IF
        OVER @ SWAP 1- ROT SWAP
        [ DUP I @ J @ SWAP ! ]
        DO I DROP LOOP
    THEN
    DROP ;

\ Palabra que crea un nuevo array con los valores únicos del array original.
: UNICOS ( array -- array )
    ARRAY-VACIO
    DUP LARGO 0 DO
        I OBTENER BUSCAR
        IF DROP ELSE AGREGAR THEN
    LOOP
    DROP ;

: MAIN
    10 NUEVO-ARRAY
    1 2 3 4 5 6 7 8 9 42 AGREGAR
    .CR "Array original: " MOSTRAR
    .CR
    INVERTIR
    .CR "Array invertido: " MOSTRAR
    .CR
    ORDENAR
    .CR "Array ordenado: " MOSTRAR
    .CR
    .CR "Buscando el valor 42: " 42 BUSCAR .NUMBER
    .CR
    .CR "Eliminando el valor 42: " 42 ELIMINAR
    .CR "Array después de eliminar 42: " MOSTRAR
    .CR
    .CR "Array único: " UNICOS MOSTRAR ;

```