```forth
: escribir-mensaje-saludo ( -- )
  "Hola, mundo." cr
;
: suma-matrices ( -- )
  2dup @ @ + 2drop rot @ @ +
;
: factorial ( n -- n! )
  dup 0> if drop 1
  else 1 swap while dup 1- * swap repeat drop
  then
;
: fibonacci ( n -- n-ésimo Fibonacci )
  dup 0= if drop 0
  else dup 1= if drop 1
  else dup 2= if drop 1
  else nip swap fibonacci fibonacci +
  then then then
;
: es-primo? ( n -- boolean )
  dup 2> if
    dup 3< if
      2dup @ @ while
        2dup > swap mod 0= if drop false exit then
        2dup @ + 1+ swap repeat drop
      then
    else true
    then
  else false
  then
;
: lista-primos ( n -- )
  1 2 do i is-primo? if i .r then loop drop
;
: saludo-personalizado ( nombre -- )
  "Hola, " swap .r "!" cr
;
: sumar-hasta ( n -- sumatoria )
  begin 0 swap
    dup 0> while
      tuck +
      1+
    repeat drop
  enddrop
;
: mostrar-triangulo-pascal ( n -- )
  1 do i dup loop cr
  0 1 do i dup
    2dup dup 1+ mod if @ @ + else drop drop 0 then
    .r
  loop drop
;
: mostrar-tabla-multiplicar ( n -- )
  1 n do i 1 n do j i j * .r loop loop cr
;
: ordenar-lista ( -- )
  begin dup length 1> while
    0 1+ do i swap i + over >= while
      tuck over @ swap @ swap !
      1+
    repeat drop
    1- swap rot rot tail recurse
  enddrop
;
```

El código anterior contiene una variedad de funciones y procedimientos útiles en FORTH.

* `escribir-mensaje-saludo`: Imprime el mensaje "Hola, mundo." en la consola.
* `suma-matrices`: Suma dos matrices de 2x2.
* `factorial`: Calcula el factorial de un número.
* `fibonacci`: Calcula el n-ésimo número de Fibonacci.
* `es-primo?`: Comprueba si un número es primo.
* `lista-primos`: Imprime los números primos hasta un número dado.
* `saludo-personalizado`: Saluda a una persona por su nombre.
* `sumar-hasta`: Calcula la sumatoria de los números desde 1 hasta un número dado.
* `mostrar-triangulo-pascal`: Muestra el triángulo de Pascal hasta un número dado de filas.
* `mostrar-tabla-multiplicar`: Muestra la tabla de multiplicar para un número dado.
* `ordenar-lista`: Ordena una lista de números en orden ascendente.