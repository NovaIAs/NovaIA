```forth
: suma ( n1 n2 -- suma )
   + ;

: resta ( n1 n2 -- resta )
   - ;

: multiplica ( n1 n2 -- producto )
   * ;

: divide ( n1 n2 -- cociente )
   / ;

: modulo ( n1 n2 -- resto )
   mod ;

: potencia ( base exponente -- potencia )
   ^ ;

: factorial ( n -- factorial )
   1, do
      dup i -1 *
   loop drop ;

: fibonacci ( n -- fibonacci )
   dup 0 1 do
      swap over +
   loop drop ;

: mayor-que ( n1 n2 -- resultado )
   > ;

: menor-que ( n1 n2 -- resultado )
   < ;

: igual-que ( n1 n2 -- resultado )
   = ;

: distinto-que ( n1 n2 -- resultado )
   <> ;

: mayor-o-igual-que ( n1 n2 -- resultado )
   >= ;

: menor-o-igual-que ( n1 n2 -- resultado )
   <= ;

: abs ( n -- valor-absoluto )
   dup 0 < if neg else drop then ;

: max ( n1 n2 -- maximo )
   over max ;

: min ( n1 n2 -- minimo )
   over min ;

: even ( n -- resultado )
   2 mod 0 = ;

: odd ( n -- resultado )
   2 mod 0 <> ;

: tabla-multiplicar ( n -- tabla-multiplicar )
   1, 10 do
      i 'x' . 1, i do
         i dup * .
      loop cr
   loop ;

: es-primo ( n -- resultado )
   2 < if
      2 2 swap do
         dup mod 0 <> if exit then
         2 dup +
      loop
      drop true
   else
      false
   then ;

: lista-primos ( n -- lista-primos )
   1 100 do
      es-primo if i . then
   loop cr ;
```

Este código implementa varias funciones matemáticas y de uso general en el lenguaje de programación FORTH. Las funciones son:

* `suma`: suma dos números.
* `resta`: resta dos números.
* `multiplica`: multiplica dos números.
* `divide`: divide dos números.
* `modulo`: calcula el resto de la división de dos números.
* `potencia`: eleva un número a una potencia.
* `factorial`: calcula el factorial de un número.
* `fibonacci`: calcula el número de Fibonacci de un número.
* `mayor-que`: compara dos números y devuelve si el primero es mayor que el segundo.
* `menor-que`: compara dos números y devuelve si el primero es menor que el segundo.
* `igual-que`: compara dos números y devuelve si son iguales.
* `distinto-que`: compara dos números y devuelve si son distintos.
* `mayor-o-igual-que`: compara dos números y devuelve si el primero es mayor o igual que el segundo.
* `menor-o-igual-que`: compara dos números y devuelve si el primero es menor o igual que el segundo.
* `abs`: calcula el valor absoluto de un número.
* `max`: devuelve el máximo de dos números.
* `min`: devuelve el mínimo de dos números.
* `even`: comprueba si un número es par.
* `odd`: comprueba si un número es impar.
* `tabla-multiplicar`: imprime la tabla de multiplicar de un número.
* `es-primo`: comprueba si un número es primo.
* `lista-primos`: imprime una lista de los números primos entre 1 y 100.

El código también incluye una función `do` que permite iterar sobre una serie de números. La función `do` toma dos argumentos: el número inicial y el número final. La función ejecuta el código entre `do` y `loop` una vez por cada número entre el número inicial y el número final.

El código también incluye una función `if` que permite ejecutar código condicionalmente. La función `if` toma dos argumentos: la condición y el código a ejecutar si la condición es verdadera. El código también incluye una función `else` que permite ejecutar código si la condición es falsa.

El código también incluye una función `swap` que intercambia los dos números en la pila. La función `swap` es útil para cambiar el orden de los números en la pila.

El código también incluye una función `drop` que elimina el número superior de la pila. La función `drop` es útil para eliminar números no deseados de la pila.

El código también incluye una función `cr` que imprime un salto de línea. La función `cr` es útil para imprimir resultados en la consola.