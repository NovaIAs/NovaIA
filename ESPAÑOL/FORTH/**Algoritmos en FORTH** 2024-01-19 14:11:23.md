```forth
: FACTORIAL ( n -- factorial )
  1 SWAP
  BEGIN
    0> WHILE
      SWAP 1- *
    REPEAT
    DROP ;

: FIBONACCI ( n -- fibonacci )
  DUP 0= IF 0 EXIT THEN
  DUP 1= IF 1 EXIT THEN
  2DUP - FACTORIAL / 2DUP - FACTORIAL / + ;

: ALEATORIO ( -- random )
  TIME MOD 65536 ;

: BÚSQUEDA-BINARIA ( lista valor )
  2SWAP
  BEGIN
    OVER 2DUP SWAP < OVER WHILE
      2DUP + 2 / SWAP
    REPEAT
    DROP ;

: ORDENAR ( lista )
  BEGIN
    DUP 0 DO
      SWAP 2DUP + 2 / OVER I > IF SWAP THEN
    LOOP
    DROP ;

: PRIMERO ( lista -- valor )
  2DUP 1- SWAP @ ;

: ÚLTIMO ( lista -- valor )
  DUP 1- @ ;

: PARTICIÓN ( lista pivote )
  0 DUP [R] VAR
  [L] 0 VAR
  [i] 0 VAR
  BEGIN
    DUP ?DO
      DUP @ >R @ IF [R] [L] @ + 2 / 2SWAP SWAP @ SWAP ! THEN
      [L] [L] 1+ !
      [i] [i] 1+ !
    LOOP
    DROP
  R> @ [R] SWAP @ SWAP !
  R@ ;

: ORDENAR-RÁPIDO ( lista )
  DUP LENGTH 1> IF
    DUP 0 ALEATORIO PARTICIÓN
    DUP ?DO
      DUP LENGTH 1> IF
        [R>] 1+ SWAP @ ORDENAR-RÁPIDO
        [L>] 1- SWAP @ ORDENAR-RÁPIDO
      THEN
    LOOP
    DROP
  THEN ;

: IMPRIMIR ( -- )
  BEGIN
    DUP ?DO
      DUP TYPE @ CR ." "
    LOOP
    TYPE ." -> "
    DUP TYPE @ CR ." "
    DROP ;

100 FACTORIAL IMPRIMIR
1000 FIBONACCI IMPRIMIR
57 ALEATORIO IMPRIMIR
[1 2 3 4 5 6 7 8 9 10] BÚSQUEDA-BINARIA 5 IMPRIMIR
[10 9 8 7 6 5 4 3 2 1] ORDENAR IMPRIMIR
[1 7 3 11 13 6 5 9 2 8] ORDENAR-RÁPIDO IMPRIMIR
```

Explicación:

* La primera parte del código define varias funciones útiles:

    * FACTORIAL: Calcula el factorial de un número.
    * FIBONACCI: Calcula el número de Fibonacci de un índice.
    * ALEATORIO: Devuelve un número aleatorio.
    * BÚSQUEDA-BINARIA: Realiza una búsqueda binaria en una lista ordenada.
    * ORDENAR: Ordena una lista de números en orden ascendente.
    * PRIMERO: Devuelve el primer elemento de una lista.
    * ÚLTIMO: Devuelve el último elemento de una lista.
    * PARTICIÓN: Particiona una lista alrededor de un pivote.
    * ORDENAR-RÁPIDO: Ordena una lista de números utilizando el algoritmo de ordenación rápida.

* La segunda parte del código utiliza estas funciones para realizar varias operaciones:

    * Calcula el factorial de 100.
    * Calcula el número de Fibonacci de 1000.
    * Genera un número aleatorio entre 0 y 65535.
    * Realiza una búsqueda binaria en una lista de números ordenados para encontrar el índice del número 5.
    * Ordena una lista de números desordenados.
    * Ordena una lista de números desordenados utilizando el algoritmo de ordenación rápida.