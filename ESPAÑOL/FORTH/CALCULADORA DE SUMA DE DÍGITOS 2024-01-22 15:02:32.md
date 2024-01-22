```forth

: ENCONTRAR-SUMA-DIGITOS ( n -- n )
  0 >LOOP [
    SWAP MOD 1 +
    ROT >R
    SWAP 10 / R>
  ] R> ;

: ENCONTRAR-SUMA-FACTORIAL-DIGITOS ( n -- n )
  0 DO [
    SWAP MOD F>NUMBER!
    ROT >R
    SWAP 10 / R>
  ] LOOP
  NUMBER@ ENCONTRAR-SUMA-DIGITOS ;

: ENCONTRAR-SUMA-CUADRADO-DIGITOS ( n -- n )
  0 DO [
    SWAP MOD DUP *
    ROT >R
    SWAP 10 / R>
  ] LOOP
  NUMBER@ ENCONTRAR-SUMA-DIGITOS ;

: ENCONTRAR-SUMA-CUBO-DIGITOS ( n -- n )
  0 DO [
    SWAP MOD DUP DUP *
    ROT >R
    SWAP 10 / R>
  ] LOOP
  NUMBER@ ENCONTRAR-SUMA-DIGITOS ;

: ENCONTRAR-SUMA-POTENCIA-DIGITOS ( n m -- n )
  0 DO [
    SWAP MOD DUP m POWER
    ROT >R
    SWAP 10 / R>
  ] LOOP
  NUMBER@ ENCONTRAR-SUMA-DIGITOS ;

: CALCULADORA-SUMA-DIGITOS ( "Calculadora de suma de dígitos" )
  ." Ingrese un número: " NUMBER!
  ." Suma de los dígitos: "
  NUMBER@ ENCONTRAR-SUMA-DIGITOS .CR
  ." Suma de los cuadrados de los dígitos: "
  NUMBER@ ENCONTRAR-SUMA-CUADRADO-DIGITOS .CR
  ." Suma de los cubos de los dígitos: "
  NUMBER@ ENCONTRAR-SUMA-CUBO-DIGITOS .CR
  ." Suma de los factoriales de los dígitos: "
  NUMBER@ ENCONTRAR-SUMA-FACTORIAL-DIGITOS .CR
  ." Suma de los dígitos elevados a la potencia de 3: "
  NUMBER@ 3 ENCONTRAR-SUMA-POTENCIA-DIGITOS .CR ;

```