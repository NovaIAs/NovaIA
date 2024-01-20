```forth

: factorial ( n -- result )
  dup 1=
  if drop 1
  else swap 1- factorial swap *
  then ;

: fibonacci ( n -- result )
  dup 1>
  if drop 1
  else swap 1- fibonacci dup +
  then ;

: lcm ( a b -- result )
  dup max swap 0 swap >
  if drop swap
  else swap mod
  then ;

: gcd ( a b -- result )
  dup max swap 0 swap >
  if drop swap
  else swap mod lcm
  then ;

: prime? ( n -- flag )
  1> if
    drop false
  else 2> if
    drop true
  else 2- dup
    1+ 2do
    mod 0=
    or
    loop
    tuck not
  then then ;

: prime-factorize ( n -- primes )
  dup 1>
  if drop 0
  else 2> if
    drop 2
  else 2- 1+
    [ mod 0= ]
    while
    [ drop 2dup lcm
      [ mod 0= ]
      while
      [ tuck ]
      until
    ]
    repeat
  then then ;

: divisors ( n -- divisors )
  drop 1+
  2dup >
  if drop 1
  else 2- 1+
    [ mod 0= ]
    while
    [ tuck 1+ ]
    until
  then ;
```

Este código Forth implementa varias funciones matemáticas básicas, que incluyen factoriales, números de Fibonacci, mínimo común múltiplo (MCM), máximo común divisor (MCD), comprobación de primalidad y factorización prima.

* **factorial** calcula el factorial de un número entero no negativo.
* **fibonacci** calcula el número de Fibonacci de un número entero no negativo.
* **lcm** calcula el mínimo común múltiplo de dos números enteros.
* **gcd** calcula el máximo común divisor de dos números enteros.
* **prime?** comprueba si un número entero es primo.
* **prime-factorize** factoriza un número entero en sus factores primos.
* **divisors** enumera todos los divisores de un número entero.

Este código se puede utilizar para una variedad de tareas, como la criptografía, la teoría de números y las matemáticas aplicadas.