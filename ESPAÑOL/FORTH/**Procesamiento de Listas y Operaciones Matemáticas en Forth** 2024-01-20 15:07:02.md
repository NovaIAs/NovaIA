```forth

: REPETIR (  " número de veces  palabra1 palabra2 ..." -- ... )
  BEGIN
    DUP 0> WHILE
      DROP 1- ROT EXECUTE
    REPEAT ;

: SUMAR (  " n -- n1 n2 ... nn suma" )
  2OVER @ WHILE
    OVER + SWAP ; DROP ;

: FACTORIAL (  " n -- n! " )
  1 OVER = IF DROP ELSE 1 SWAP REPETIR NIP * THEN ;

: POTENCIA (  " x n -- x^n " )
  BEGIN 0 > WHILE
    DUP 0> IF EXIT THEN
    SWAP 2 * SWAP NI* ROT ;

: PRIMERO (  " lista -- primer elemento"  )
  SWAP @ ;

: RESTO (  " lista -- resto de la lista (sin el primer elemento)"  )
  SWAP OVER 2SWAP 2DROP ;

: LONGITUD (  " lista -- nº de elementos en la lista"  )
  0 BEGIN
    NIP DUP = 0 = OR WHILE
      1+ REPEAT DROP ;

: ORDENAR (  " lista -- lista ordenada"  )
  BEGIN
    LONGITUD OVER 1 SUB SWAP
    PRIMERO SWAP 2DUP OVER
    WHILE
      2DUP > IF
        SWAP 2SWAP 2OVER THEN
    REPEAT
    RESTO 2SWAP RECURSIVE CALL ORDENAR SWAP APPEND ;
  RECURSE ;

: GCD (  " m n -- mcd(m,n) " )
  BEGIN
    MOD 0= WHILE
      SWAP DROP
    REPEAT
    SWAP ;

: LCM (  " m n -- mcm(m,n) " )
  2DUP / NIP SWAP * ;

```

Explicación:

- `REPETIR`: Repite una palabra especificada un número de veces determinado.

- `SUMAR`: Suma los elementos de una lista.

- `FACTORIAL`: Calcula el factorial de un número.

- `POTENCIA`: Eleva un número a una potencia especificada.

- `PRIMERO`: Obtiene el primer elemento de una lista.

- `RESTO`: Obtiene el resto de una lista sin el primer elemento.

- `LONGITUD`: Obtiene el número de elementos en una lista.

- `ORDENAR`: Ordena una lista en orden ascendente.

- `GCD`: Calcula el máximo común divisor de dos números.

- `LCM`: Calcula el mínimo común múltiplo de dos números.