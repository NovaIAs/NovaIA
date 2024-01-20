```forth
: factorial ( n -- r )
  1         do i ->r r * loop
  drop ;

: esPrimo? ( n -- flag )
  2 swap 1 - 2 / 1 + do
    i dup factorial swap mod 0=
  until drop ;

: menoresPrimos ( n -- n )
  1 do
    i esPrimo? if drop else dup . space fi
  loop
  drop ;

: primosSumatoria ( n -- sum )
  1 2 rotate do
    i esPrimo? if i + else drop fi
  loop
  drop ;

: sumaPrimos ( n -- sum )
  1 2 rotate do
    i esPrimo? if i + else drop fi
  loop
  drop ;

: fibonacci ( n -- fib )
  1 cells allot reserve
  0 1 do i 2cells+ dup @ swap @ + cell+ loop
  drop cell>r ;

: cuadradosSumatoria ( n -- sum )
  1 2 rotate do
    i swap * cell+ loop
    drop ;

: cuadradosMedia ( n -- m )
  1 2 rotate do
    i swap * cell+ loop
    over 2 cells+ 2 / cell>r ;

: mayorElemento ( n -- m )
  0 do
    i over < if drop else dup cell>r fi
  loop
  drop ;

: menorElemento ( n -- m )
  0 do
    i over > if drop else dup cell>r fi
  loop
  drop ;

: mediaElemento ( n -- m )
  0 do
    i over + cell+ loop
    over 2 / cell>r ;

: sumaElementos ( n -- sum )
  0 do
    i over + cell+ loop
    drop ;
```

Este código Forth implementa una variedad de funciones matemáticas y de procesamiento de listas, incluyendo:

* `factorial`: Calcula el factorial de un número.
* `esPrimo?`: Comprueba si un número es primo.
* `menoresPrimos`: Imprime los números primos menores que un número dado.
* `primosSumatoria`: Suma los números primos menores que un número dado.
* `fibonacci`: Calcula el n-ésimo número de Fibonacci.
* `cuadradosSumatoria`: Suma los cuadrados de los números de 1 a un número dado.
* `cuadradosMedia`: Calcula la media de los cuadrados de los números de 1 a un número dado.
* `mayorElemento`: Encuentra el mayor elemento de una lista de números.
* `menorElemento`: Encuentra el menor elemento de una lista de números.
* `mediaElemento`: Calcula la media de una lista de números.
* `sumaElementos`: Suma los elementos de una lista de números.