```forth
: factorial ( n -- n! )
  dup
  1 do
    dup
    swap
    *
    swap
  loop
  drop;

: potencia ( base exponente -- potencia )
  dup
  1 1 do
    swap
    *
    drop
  loop
  drop;

: fibonacci ( n -- fibonacci(n) )
  dup
  0 = if
    1
  else
    dup
    1 -
    fibonacci
    dup
    2 -
    fibonacci
    +
  then;

: mcd ( m n -- mcd(m,n) )
  dup
  0 = if
    swap
  else
    swap
    dup
    mod
    mcd
  then;

: mcm ( m n -- mcm(m,n) )
  over
  dup
  mcd
  /
  *
  drop;

: primo? ( n -- booleano )
  1 if
    true
  else
    dup
    2 > if
      2 swap do
        dup
        mod
        0 = if
          false
        then
        swap
        1 +
      loop
      drop
      true
    else
      false
    then
  then;

: lista-primos ( n -- lista de primos desde 1 hasta n )
  dup
  1 +
  do
    primo?
    if
      swap
      .
    then
    1 +
  loop
  drop;

: tabla-multiplicar ( n -- tabla de multiplicar de n )
  do
    i .
    i 1+ swap do
      * .
    loop
    cr
  loop
  drop;
```

Explicación del código:

* **factorial**: Calcula el factorial de un número.
* **potencia**: Calcula la potencia de un número a un exponente dado.
* **fibonacci**: Calcula el número de Fibonacci de un índice dado.
* **mcd**: Calcula el máximo común divisor de dos números.
* **mcm**: Calcula el mínimo común múltiplo de dos números.
* **primo?**: Comprueba si un número es primo.
* **lista-primos**: Crea una lista de números primos desde 1 hasta un número dado.
* **tabla-multiplicar**: Genera una tabla de multiplicar para un número dado.

Este código es complejo y variado, y cubre una amplia gama de temas matemáticos. Es un buen ejemplo de las capacidades del lenguaje Forth.