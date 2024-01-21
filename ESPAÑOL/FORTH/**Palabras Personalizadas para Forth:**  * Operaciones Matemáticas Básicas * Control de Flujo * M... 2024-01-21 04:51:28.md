```forth

\ Definiciones de palabras clave personalizadas
: SUMA ( n1 n2 -- suma ) 2swap + ;
: RESTA ( n1 n2 -- diferencia ) 2swap - ;
: MULT ( n1 n2 -- producto ) 2swap * ;
: DIV ( n1 n2 -- cociente ) 2swap / ;
: MOD ( n1 n2 -- residuo ) 2swap mod ;

\ Pila de llamadas
: .S ( -- a ) OVER . ;
: .2S ( -- ab ) 2OVER . DUP . ;
: .3S ( -- abc ) 3OVER . DUP . DUP . ;

\ Entrada y salida
: .CR ( -- ) CR ;
: ." ( "string" -- ) . .CR ;
: .N ( n -- n ) . .CR ;

\ Definición de palabras condicionales
: IF ( condición verdadero falso -- ¬ )
      DUP 0=
      IF DROP
      ELSE
         SWAP EXECUTE DROP
      THEN ;
: ELSE ( -- ) NIP ;
: THEN ( -- ) ;

\ Operaciones lógicas
: AND ( a b -- ab ) 0= 0= AND ;
: OR ( a b -- ab ) DUP 0= OR ;

\ Definición de palabras de control de flujo
: WHILE ( condición -- )
      DUP 0=
      IF DROP EXIT ELSE
         SWAP EXECUTE
         LIFT WHILE
      THEN ;
: REPEAT ( n -- )
      0 DO
         I @ EXECUTE
         LOOP ;

\ Definición de palabras de manipulación de datos
: SWAP ( a b -- b a ) ROT ;
: DROP ( x -- ) NIP ;
: DUP ( x -- x x ) OVER ;
: OVER ( a b -- a b a ) SWAP OVER ;

\ Definición de palabras matemáticas
: ABS ( n -- n ) DUP 0> IF NEGATE THEN ;

\ Definición de palabras de tabla (arrays)
: .T ( table-addr size -- ) BEGIN UNTIL ;
: FILL ( table-addr size value -- ) BEGIN UNTIL FILL ;

\ Definición de palabras de entrada y salida de ficheros
: OPEN-FILE ( filename access-mode -- file-handle )
      CREATE-FILE OPEN-FILE ;
: CLOSE-FILE ( file-handle -- ) CLOSE-FILE ;
: READ-FILE ( file-handle buffer size -- bytes-read )
      READ-FILE ;
: WRITE-FILE ( file-handle buffer size -- bytes-written )
      WRITE-FILE ;

\ Definición de palabras de sistema
: QUIT ( -- ) ABORT ;
: BYE ( -- ) BYE ;

```