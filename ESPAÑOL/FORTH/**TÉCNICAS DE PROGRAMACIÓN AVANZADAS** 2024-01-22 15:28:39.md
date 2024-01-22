```forth
: FÁBRICA-DE-PALABRAS ( -- )
    BEGIN
        DUP WHILE
            SWAP #\ CR
            0 DO
                I ?LOOP
            TYPE ;
            SWAP DROP
        REPEAT
    ;

: CUENTA-PALABRAS ( -- n )
    BEGIN
        DUP WHILE
            SWAP @ > IF
                DROP 1+
            THEN
            SWAP DROP
        REPEAT
        DROP
    ;

: BUSCA-PALABRA ( palabra -- n )
    BEGIN
        DUP WHILE
            SWAP @ = IF
                SWAP DROP 1+
                EXIT
            THEN
            SWAP DROP
        REPEAT
        DROP
    ;

: BORRA-PALABRA ( -- )
    BEGIN
        DUP WHILE
            SWAP @ > IF
                <> 0 DO
                    I ROLL
                LOOP
                DROP 0
                EXIT
            THEN
            SWAP DROP
        REPEAT
        DROP
    ;

\ PRIMEROS CÓDIGOS: CUENTAN, BORRAN Y BUSCAN UNA PALABRA

: SOY-PALABRA ( -- flag )
    @ @ >

: ES-PALABRA ( dirección -- flag )
    @ 0= NOT

: DESCUENTA-LARGO ( -- )
    BEGIN
        DUP WHILE
            OVER > IF
                SWAP 1-
            THEN
            SWAP DROP
        REPEAT
    ;

: CREA-ESPACIO ( n -- dirección )
    BEGIN
        DUP WHILE
            0 FILL
            1-
        REPEAT
        1+
    ;

\ LOS CÓDIGOS ANTERIORES NOS PERMITEN MANIPULAR LAS PALABRAS

: NEW ( longitud nombre dirección -- )
    BEGIN
        DUPLICAR 0 DO
            I ?LOOP
        CREATE
        OVER 1+ NAME>TO
        OVER VOCABULARY>TO
    ;

\ NUEVO CÓDIGO: CREA UNA NUEVA PALABRA

: BYE ( -- )
    BYE
;

\ NUEVA PALABRA: TERMINA EL PROGRAMA

: GREET ( -- )
    "Hola Mundo" TYPE
;

\ NUEVA PALABRA: IMPRIME "HOLA MUNDO"

: VARIABLES-COMPARTIDAS ( -- )
    0 10 VARIABLES
    0 ! 1 !
    [ @ 1+ ] 2VARIABLES
    HERE 1- ,
    DEPTH + HERE 1- ,
    @ TYPE ,
    @ TYPE
;

\ NUEVO CÓDIGO: UTILIZA VARIABLES COMPARTIDAS

: LISTA-DE-VARIABLES ( dirección cuenta -- )
    BEGIN
        DUP WHILE
            SWAP @ TYPE
        REPEAT
        DROP
    ;

\ NUEVO CÓDIGO: LISTA DE VARIABLES

: ARRAY ( longitud nombre dirección -- )
    BEGIN
        DUPLICAR 0 DO
            0 FILL
            I ?LOOP
        CREATE
        OVER 1+ NAME>TO
        OVER VOCABULARY>TO
    ;

\ NUEVO CÓDIGO: CREA UN NUEVO ARRAY

: SET ( array índice valor -- )
    SWAP @ @ + !

\ NUEVO CÓDIGO: ESTABLECE UN VALOR EN UN ARRAY

: GET ( array índice -- )
    SWAP @ @ + @

\ NUEVO CÓDIGO: DEVUELVE UN VALOR DE UN ARRAY

: LISTA-DE-ARRAYS ( -- )
    BEGIN
        DUP WHILE
            SWAP @ TYPE
        REPEAT
        DROP
    ;

\ NUEVO CÓDIGO: LISTA DE ARRAYS

: MANIPULACIÓN-DE-ARRAYS ( -- )
    16 ARRAY array
    10 ARRAY array2
    30 ARRAY array3
    1 2 3 4 5 SET array
    1 2 3 4 5 SET array2
    1 2 3 4 5 SET array3
    array LISTA-DE-ARRAYS
    array2 LISTA-DE-ARRAYS
    array3 LISTA-DE-ARRAYS
    3 GET array TYPE
    4 GET array2 TYPE
    5 GET array3 TYPE
;

\ NUEVO CÓDIGO: MANIPULACIÓN DE ARRAYS

```

Explicación:

* **FÁBRICA-DE-PALABRAS:** Crea una nueva palabra que imprime una serie de caracteres en la consola.
* **CUENTA-PALABRAS:** Cuenta el número de palabras en una cadena de caracteres.
* **BUSCA-PALABRA:** Busca una palabra en una cadena de caracteres y devuelve su posición.
* **BORRA-PALABRA:** Borra una palabra de una cadena de caracteres.
* **SOY-PALABRA:** Comprueba si una dirección de memoria contiene una palabra.
* **ES-PALABRA:** Comprueba si una dirección de memoria contiene una palabra y devuelve un valor booleano.
* **DESCUENTA-LARGO:** Resta la longitud de una palabra de una dirección de memoria.
* **CREA-ESPACIO:** Crea un espacio de memoria con una longitud determinada.
* **NEW:** Crea una nueva palabra con un nombre y una longitud determinados.
* **BYE:** Termina el programa.
* **GREET:** Imprime "Hola Mundo" en la consola.
* **VARIABLES-COMPARTIDAS:** Utiliza variables compartidas para almacenar datos.
* **LISTA-DE-VARIABLES:** Lista las variables compartidas en la consola.
* **ARRAY:** Crea un nuevo array con una longitud y un nombre determinados.
* **SET:** Establece un valor en una posición determinada de un array.
* **GET:** Obtiene el valor de una posición determinada de un array.
* **LISTA-DE-ARRAYS:** Lista los arrays en la consola.
* **MANIPULACIÓN-DE-ARRAYS:** Crea y manipula arrays.