```forth

: HOLA ( direccion -- )
BEGIN
  13 10 EMIT ;

: NUMERO ( numero direccion -- )
BEGIN
  SIGN
  CASE
    OF 0 DO
      UNSIGNED
    ELSE
      NEGATIVE
  ENDOF
  SWAP DO
    MOD 10 + 48 EMIT
    10/ LOOP ;

: NUMERO-DECIMAL ( numero direccion -- )
BEGIN
  NUMERO
  DUP IF 0 EMIT THEN HOLA ;

: CADENA ( direccion -- )
BEGIN
  DO
    DUP 0 = WHILE
      DROP EXIT
    ELSE
      EMIT 1+ LOOP ;

: PALABRA ( direccion -- )
BEGIN
  DO
    DUP = WHILE
      DROP EXIT
    ELSE
      EMIT 1+ LOOP ;

: ESPACIO ( -- ) (No euclidiano)
BLANK EMIT ;

: NUEVA-LINEA ( -- )
CR EMIT LF EMIT ;

: ESPACIO-DE-CADENA ( direccion -- )
BEGIN
  PALABRA LOOP ESPACIO ;

: POSICION-SALIDA ( -- direccion )
HERE ;

: IMPRIME-MSG ( mensaje -- )
BEGIN
  POSICION-SALIDA
  OVER CADENA
  HOLA LOOP
  ESPACIO ;

: FORMATO-NUMERO ( numero direccion -- )
BEGIN
  POSICION-SALIDA
  OVER NUMERO-DECIMAL
  HOLA LOOP
  ESPACIO ;

: TERMINO ( -- )
CR EMIT LF EMIT ;

: ** ( numero1 numero2 direccion -- )
BEGIN
  OVER * SWAP * SWAP ;

: ^ ( numero base direccion -- )
BEGIN
  OVER 1 SWAP DO 2DUP * LOOP DROP SWAP ;

: SUMA ( lista direccion -- resultado )
BEGIN
  0 SWAP DO
    ROT + LOOP SWAP ;

: PRODUCTO ( lista direccion -- resultado )
BEGIN
  1 SWAP DO
    ROT * LOOP SWAP ;

: MIN ( lista direccion -- minimo ) REDEFINE
BEGIN
  DO
    DUP OVER > WHILE
      OVER DROP NIP
    ELSE
      DROP EXIT
  ENDOF ;

: MAX ( lista direccion -- maximo ) REDEFINE
BEGIN
  DO
    DUP OVER < WHILE
      OVER DROP NIP
    ELSE
      DROP EXIT
  ENDOF ;

: BUSCAR-MAX ( lista direccion -- maximo )
BEGIN
  DUP
  0 BEGIN
    SWAP DUP ^ 1 SUB @ OVER HOLA
    DUP OVER MAX > WHILE
      1+ SWAP LOOP
    NIP DROP MIN @
  LOOP ;

: BUSCAR-MIN ( lista direccion -- minimo )
BEGIN
  DUP
  0 BEGIN
    SWAP DUP ^ 1 SUB @ OVER HOLA
    DUP OVER MIN < WHILE
      1+ SWAP LOOP
    NIP DROP MAX @
  LOOP ;

: TIPO-DE-DATO ( direccion -- tipo-de-dato )
BEGIN
  4 SWAP DO
    I C@ C@ + LOOP
  2DROP ;

: DEFINIR ( direccion -- ) (Definir una palabra Forth en memoria)
BEGIN
  HERE ,
  POSICION-SALIDA
  ,
  [COMPILE]
  HERE ?DO
  LEAVE ;

```