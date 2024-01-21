```forth
: PRIMERO  ( ( -- d ) Dada una lista, obtener el primer elemento )
  DUP FIRST ;

: ULTIMO   ( ( -- d ) Dada una lista, obtener el último elemento )
  DUP LAST ;

: LARGO    ( ( -- n ) Obtener el largo de una lista )
  DUP LENGTH ;

: BUSCAR   ( ( n d -- i ) Dada una lista y un elemento, encontrar la primera aparición del elemento en la lista )
  DUP 1+ 0DO WHILE [ SWAP I@ ] =IF EXIT THEN REPEAT DROP ;

: INVERTIR ( ( -- l ) Dada una lista, invertir el orden de sus elementos )
  1- [ [ 0DUP I@ ] ROT ] 0 WHILE DROP ;

: PRINTLN ( ( s... -- ) Imprimir una línea de texto )
  CR ." ;

: REPETIR  ( ( n s... -- ) Repetir una línea de texto n veces )
  BEGIN DUP 0DO ." REPEAT DROP ;

: FACTORIAL ( ( n -- n! ) Calcular el factorial de un número )
  1 [ DUP 1- * ] WHILE DROP ;

: SUMAR    ( ( n1 n2 -- suma ) Sumar dos números )
  + ;

: RESTAR   ( ( n1 n2 -- resta ) Restar dos números )
  - ;

: MULTIPLICAR ( ( n1 n2 -- producto ) Multiplicar dos números )
  * ;

: DIVIDIR   ( ( n1 n2 -- cociente ) Dividir dos números )
  / ;

: MODULO   ( ( n1 n2 -- residuo ) Calcular el residuo de la división de dos números )
  MOD ;

: POTENCIA  ( ( n1 n2 -- potencia ) Calcular la potencia de un número )
  ^ ;

: RAIZ     ( ( n -- raiz ) Calcular la raíz cuadrada de un número )
  SQRT ;

: MENOR    ( ( n1 n2 -- -1/0/1 ) Comparar dos números )
  < ;

: MAYOR    ( ( n1 n2 -- -1/0/1 ) Comparar dos números )
  > ;

: MAYORIGUAL ( ( n1 n2 -- -1/0/1 ) Comparar dos números )
  >= ;

: MENORIGUAL ( ( n1 n2 -- -1/0/1 ) Comparar dos números )
  <= ;

: IGUALDAD ( ( n1 n2 -- -1/0/1 ) Comparar dos números )
  = ;

: DISTINTO  ( ( n1 n2 -- -1/0/1 ) Comparar dos números )
  <> ;

: AND  ( ( n1 n2 -- and ) Realizar una operación AND entre dos números )
  AND ;

: OR   ( ( n1 n2 -- or ) Realizar una operación OR entre dos números )
  OR ;

: XOR  ( ( n1 n2 -- xor ) Realizar una operación XOR entre dos números )
  XOR ;

: NOT  ( ( n -- not ) Realizar una operación NOT sobre un número )
  NOT ;

: SI ( ( c s1 s2 -- ) Ejecutar s1 si c es verdadero, si no ejecutar s2 )
  IF THEN ELSE ;

: SI0 ( ( c s -- ) Ejecutar s si c es verdadero )
  IF THEN ;

: CUANDO ( ( s -- ) Ejecutar s )
  DO ;

: LOOPS ( ( n s -- ) Ejecutar s n veces )
  BEGIN DUP 0DO [ SWAP ] EXECUTE REPEAT DROP ;

: HASTA ( ( c s -- ) Ejecutar s hasta que c sea verdadero )
  BEGIN [ SWAP ] EXECUTE DUP WHILE AGAIN DROP ;

: SALIR    ( ( -- ) Salir del programa )
  EXIT ;

: PALABRA ( ( nombre s -- ) Definir una nueva palabra )
  CREATE , ;

: COMPILAR ( ( s -- ) Compilar una cadena de texto como código FORTH )
  INTERPRET ;

: LISTA ( ( l -- ) Imprimir los elementos de una lista, uno por línea )
  BEGIN DUP WHILE DEPTH DUP @ . CR REPEAT DROP ;

: STRING ( ( s -- c ) Convertir una cadena de caracteres en un código ASCII )
  CHAR ;

: CHAR ( ( c -- s ) Convertir un código ASCII en una cadena de caracteres )
  CHR ;
```