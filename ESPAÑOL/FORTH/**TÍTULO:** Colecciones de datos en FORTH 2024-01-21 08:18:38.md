```
: Crear-Lista (Tamaño -- Lista)
  0 DO
    SWAP
    OVER @
    LOOP
  DROP ;

: Llenar-Lista (Lista -- Lista)
  0 DO
    DUP
    OVER @
    3 +
    SWAP !
    LOOP
  DROP ;

: Imprimir-Lista (Lista -- )
  BEGIN
    DUP
    WHILE
      OVER @ .
      SWAP 1+ SWAP
    REPEAT
    DROP ;

: Buscar-Mayor (Lista -- Valor)
  BEGIN
    DUP
    WHILE
      DUP @ >@
      SWAP
    REPEAT
    DROP ;

: Ordenar-Lista (Lista -- Lista)
  BEGIN
    DUP
    WHILE
      SWAP Buscar-Mayor @TUCK
      SWAP OVER
    REPEAT
    DROP ;

: Permutar-Elementos (Lista -- Lista)
  BEGIN
    2DUP @ OVER @ = IF
      DROP 2DROP
    ELSE
      OVER @ OVER + 1 + SWAP @ >@
      SWAP 2DUP @ 2DROP
    THEN
  REPEAT ;

: Combinar-Listas (Lista1 Lista2 -- Lista)
  BEGIN
    DUP
    WHILE
      DUP @ OVER @ SWAP
      Permutar-Elementos
      SWAP
    REPEAT
    DROP ;

: Entero->Ascii (Entero -- Cadena)
  BEGIN
    DUP
    WHILE
      OVER 48 + CHAR
      SWAP
    REPEAT
    DROP ;

: Imprimir-Entero (Entero -- )
  Entero->Ascii @ TYPE ;

: Intercambiar-2Valores (A B -- B A)
  SWAP ;

: Ordenar-Lista-Por-Valor (Lista -- Lista)
  BEGIN
    DUP
    WHILE
      SWAP
      DUP
      WHILE
        DUP @ >@
      REPEAT
      Intercambiar-2Valores
    REPEAT
    DROP ;

: Fibonacci (N -- Valor)
  BEGIN
    DUP 0= IF
      1
    ELSE
      DROP 1- Fibonacci 2- Fibonacci +
    THEN ;

: Calcular-Factorial (N -- Factorial)
  BEGIN
    DUP 1= IF
      1
    ELSE
      DROP 1- Calcular-Factorial *
    THEN ;

: Hipotenusa (Cat-A Cat-B -- Hipotenusa)
  DUP * DUP * + SQR ;

: Desviacion-Estandar (Lista -- Desviacion)
  BEGIN
    DUP
    WHILE
      SWAP SUM
    REPEAT
    DROP
    2/
    Calcular-Factorial SQR ;

: Crear-Matriz (Filas Columnas -- Matriz)
  BEGIN
    DUP
    WHILE
      DUP
      WHILE
        0
      REPEAT
      LOOP
    REPEAT
    DROP ;

: Elemento-Matriz (Matriz Nº-Fila Nº-Columna -- Elemento)
  BEGIN
    DUP C@ -
    DUP * OVER C@ * +
    C@ +
  THEN ;

: Definir-Elemento-Matriz (Matriz Nº-Fila Nº-Columna Valor -- )
  BEGIN
    DUP C@ -
    DUP * OVER C@ * +
    C@ +
    SWAP !
  THEN ;

: Imprimir-Matriz (Matriz -- )
  BEGIN
    DUP
    WHILE
      DUP @
      WHILE
        SWAP .
      REPEAT
      TYPE CRLF
      1+ SWAP
    REPEAT
    DROP ;

: Matriz-Transpuesta (Matriz -- Matriz-Transpuesta)
  BEGIN
    C@ R@ DO
      I J DO
        Elemento-Matriz @
      LOOP
      LOOP
    DROP ;

: Multiplicar-Matrices (Matriz1 Matriz2 -- Matriz-Producto)
  BEGIN
    R@ C@ DO
      I J DO
        I 1+ Elemento-Matriz @ K DO
          OVER * +
        LOOP
        K 1+
      LOOP
      DUP @ !
    LOOP
    SWAP R@ C@ 2DROP
  THEN ;

: Rango (Inicio Fin -- Lista)
  BEGIN
    DUP
    WHILE
      SWAP .
      1+ SWAP
    REPEAT ;

: Buscar-Valor (Lista Valor -- Indice)
  BEGIN
    DUP
    WHILE
      DUP @ = IF
        DROP
      ELSE
        1+ SWAP
      THEN
    REPEAT
    DROP ;
```