```forth

: escribir ( n -- )  " Escribe un número en la consola. "
     ." cr

: fib ( n -- m )  " Calcula el n-ésimo número de Fibonacci. "
   dup 0 < if
    drop 0
  else
    swap 1 - fib swap 1 + +
  then

: primo? ( n -- f )  " Comprueba si un número es primo. "
  dup 1 = if
    drop true
  else
    2 > while
      dup swap mod 0 = if
        drop false exit
      else
        1 +
      then
    repeat drop true
  then

: factores ( n -- )  " Muestra los factores de un número. "
  2 begin
    dup mod 0 = while
      dup ." cr
      over /
    repeat drop

: es-perfecto? ( n -- f )  " Comprueba si un número es perfecto. "
  dup 1 + 2dup - factores + =

: suma-perfectos ( n -- s )  " Suma los números perfectos menores que n. "
  dup 0 do
    dup es-perfecto? if
      +
    else
      drop
    then
  loop drop

: es-primo? ( n -- f )  " Comprueba si un número es primo. "
  dup 1 = if
    drop false
  else
    dup 2 sqrt 1 + swap begin
      dup >= while
        mod 0 = if
          drop false exit
        else
          1 +
        then
      repeat drop true
    then

: factores-primos ( n -- )  " Muestra los factores primos de un número. "
  dup 2 begin
    dup mod 0 = while
      dup ." cr
      over /swap
    repeat drop

: suma-primos ( n -- s )  " Suma los números primos menores que n. "
  dup 0 do
    dup es-primo? if
      +
    else
      drop
    then
  loop drop

```