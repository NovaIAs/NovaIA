```forth
: TRANSLATE ( string -- string )
  BEGIN
    DUP 0 DO
      2SWAP CELL@ 2 CELLS +!
    LOOP
    DROP ;

: .WORD ( addr -- )
  DUP >R @ R> . ;

: .STRING ( addr -- )
  BEGIN
    DUP @ 0= UNTIL
      SWAP 1+ SWAP .
    LOOP
    DROP ;

: #>FILE ( addr len -- )
  BEGIN
    DUP @ 0= UNTIL
      DUP @ #> 1+ SWAP .
    LOOP
    DROP ;

: #>STRING ( addr len -- )
  BEGIN
    DUP @ 0= UNTIL
      DUP @ #> 1+ SWAP .
    LOOP
    DROP ;

: HEX ( num -- string )
  BEGIN
    DUP 0>= UNTIL
      48 + ROT SWAP 16 / SWAP
    LOOP
    DROP ;

: $ ( num -- string )
  BEGIN
    DUP 0>= UNTIL
      32 + SWAP 10 / SWAP
    LOOP
    DROP ;

: DECIMAL ( num -- string )
  BEGIN
    DUP 0>= UNTIL
      48 + SWAP 10 / SWAP
    LOOP
    DROP ;

: BIN ( num -- string )
  BEGIN
    DUP 0>= UNTIL
      48 + SWAP 2 / SWAP
    LOOP
    DROP ;

: OCTAL ( num -- string )
  BEGIN
    DUP 0>= UNTIL
      48 + SWAP 8 / SWAP
    LOOP
    DROP ;

: SPACE ( n -- )
  BEGIN
    DUP 0> WHILE
      EMIT
    REPEAT
    DROP ;

: . ( string -- )
  BEGIN
    DUP @ 0= UNTIL
      SWAP @ EMIT 1+ SWAP
    LOOP
    DROP ;

: .CR ( -- )
  13 EMIT ;

: .LF ( -- )
  10 EMIT ;

: .CRLF ( -- )
  13 10 EMIT ;

: ? ( string -- )
  BEGIN
    DUP @ 0= UNTIL
      SWAP @ TYPE 1+ SWAP
    LOOP
    DROP ;

: KEY ( -- char )
  BEGIN
    KEY? WHILE
      REPEAT
    KEY
  REPEAT ;

: KEY? ( -- flag )
  128 INKEY ;

: TYPE ( char -- )
  EMIT ;

: PAUSE ( -- )
  BEGIN
    KEY? WHILE
      REPEAT
    REPEAT ;

: WAIT ( ms -- )
  BEGIN
    DUP 0> WHILE
      MS @ 1 - SWAP MS !
      BEGIN
        KEY? WHILE
          REPEAT
        REPEAT
      WHILE
    REPEAT
    DROP ;

: RANDOM ( n -- n1 )
  RANDOM 255 AND SWAP / ;

: ABS ( n -- n1 )
  DUP 0< IF
    NEGATE
  THEN ;

: MAX ( n1 n2 -- n3 )
  DUP > IF
    DROP DUP
  ELSE
    SWAP
  THEN ;

: MIN ( n1 n2 -- n3 )
  DUP > IF
    DUP
  ELSE
    SWAP
  THEN ;

: MOD ( n1 n2 -- n3 )
  SWAP 2SWAP 1- ABS ;

: S>D ( n1 -- n2 )
  D@ + SWAP ! ;

: D>S ( n1 -- n2 )
  D@ SWAP - ! ;

: S>X ( n1 -- )
  X>R R@ +! X>R 1+! ;

: X>S ( n1 -- )
  X>R @ SWAP - X>R 1-! ;

: R>X ( -- n1 )
  X>R @ ;

: X>R ( n1 -- )
  X>R ! ;

: R>S ( -- n1 )
  R@ ;

: S>R ( n1 -- )
  R@ +! ;

: S" ( string -- addr len )
  CREATE TEMP 1+ ALLOT
  BEGIN
    DUP @ 0= UNTIL
      TEMP C! 1+ TEMP @ 1+! C@
    LOOP
    TEMP @ 1- SWAP TEMP @ -
  THEN
  DROP TEMP ;

: ." ( string -- )
  BEGIN
    DUP @ 0= UNTIL
      SWAP @ EMIT 1+ SWAP
    LOOP
    DROP ;

: FIND ( string1 string2 -- addr )
  BEGIN
    DUP @ 0= UNTIL
      DUP 2SWAP C@ 2SWAP 0 DO
        OVER 2SWAP C@ = UNTIL
      1+ SWAP
    LOOP
    DROP ;

: MATCH ( string1 string2 -- flag )
  BEGIN
    DUP @ 0= UNTIL
      DUP 2SWAP C@ 2SWAP 0 DO
        OVER 2SWAP C@ = UNTIL
      1+ SWAP
    LOOP
    0= ;

: COMPARE ( string1 string2 -- n )
  BEGIN
    DUP @ 0= UNTIL
      DUP 2SWAP C@ 2SWAP 0 DO
        OVER 2SWAP C@ = UNTIL
      SWAP 1- SWAP
    LOOP
    DROP ;

: INSTRING ( string1 string2 -- flag )
  BEGIN
    DUP @ 0= UNTIL
      DUP FIND OVER 0= UNTIL
    LOOP
    DROP ;

: SUBSTRING ( string1 n1 n2 -- string2 )
  BEGIN
    SWAP DUP OVER - ALLOT
    CREATE TEMP SWAP 1+ ALLOT
    BEGIN
      DUP @ 0= UNTIL
        TEMP C! 1+ TEMP @ 1+! C@
      LOOP
      TEMP @ 1- SWAP TEMP @ -
    THEN
    DROP TEMP
  THEN
  DROP ;

: PAD ( string1 string2 n -- string3 )
  BEGIN
    SWAP DUP OVER - ALLOT
    CREATE TEMP 1+ ALLOT
    BEGIN
      DUP @ 0= UNTIL
        TEMP C! 1+ TEMP @ 1+! C@
      LOOP
      TEMP @ 1- SWAP TEMP @ -
    THEN
    SWAP OVER DUP SUBSTRING PAD PAD
    DROP TEMP
  THEN
  DROP ;

: LEFT ( string1 n -- string2 )
  BEGIN
    SWAP DUP OVER - ALLOT
    CREATE TEMP 1+ ALLOT
    BEGIN
      DUP @ 0= UNTIL
        TEMP C! 1+ TEMP @ 1+! C@
      LOOP
      TEMP @ 1- SWAP TEMP @ -
    THEN
    DROP TEMP
  THEN
  DROP ;

: RIGHT ( string1 n -- string2 )
  BEGIN
    CREATE TEMP OVER ALLOT
    BEGIN
      DUP @ 0= UNTIL
        TEMP C! 1+ TEMP @ 1+! C@
      LOOP
      TEMP @ 1- SWAP TEMP @ -
    THEN
    DROP TEMP
  THEN
  DROP ;

: MID ( string1 n1 n2 -- string2 )
  BEGIN
    SWAP DUP OVER - ALLOT
    CREATE TEMP 1+ ALLOT
    BEGIN
      DUP @ 0= UNTIL
        TEMP C! 1+ TEMP @ 1+! C@
      LOOP
      TEMP @ 1- SWAP TEMP @ -
    THEN
    SWAP OVER DUP SUBSTRING PAD PAD
    DROP TEMP
  THEN
  DROP ;

: UCASE ( string1 -- string2 )
  BEGIN
    DUP @ 0= UNTIL
      OVER C@ 97 <= AND 122 >= IF
        SWAP 32 - SWAP
      THEN 1+ SWAP
    LOOP
    DROP ;

: LCASE ( string1 -- string2 )
  BEGIN
    DUP @ 0= UNTIL
      OVER C@ 65 <= AND 90 >= IF
        SWAP 32 + SWAP
      THEN 1+ SWAP
    LOOP
    DROP ;

: TRIM ( string1 -- string2 )
  BEGIN
    CREATE TEMP OVER ALLOT
    BEGIN
      DUP @ 0= UNTIL
        TEMP C! 1+ TEMP @ 1+! C@
      LOOP
      TEMP @ 1- SWAP TEMP @ -
    THEN
    DROP TEMP
    BEGIN
      DUP @ 0= UNTIL
        C@ 32 = UNTIL
          1+ SWAP
        LOOP
        1- SWAP
      LOOP
      DUP @ 0= UNTIL
        SWAP @ 32 = UNTIL
          1- SWAP
        LOOP
        1+ SWAP
      LOOP
      DROP
    THEN
  THEN
  DROP ;

: TOKEN ( string1 -- string2 )
  BEGIN
    CREATE TEMP OVER ALLOT
    BEGIN
      DUP @ 0= UNTIL
        TEMP C! 1+ TEMP @ 1+! C@
      LOOP
      TEMP @ 1- SWAP TEMP @ -
    THEN
