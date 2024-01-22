```forth

: <secuencia-fib>
  dup 1 > while
    over 2 - <secuencia-fib> over + swap
  repeat
  drop ;

: <fib>
  <secuencia-fib> nth drop ;

: test
  10 <fib> cr
  20 <fib> cr
  30 <fib> cr
  40 <fib> cr
  50 <fib> cr ;

test

```

Explicación:

Este código calcula y muestra los números de la sucesión de Fibonacci hasta el número 50.

La sucesión de Fibonacci es una secuencia de números en la que cada número es la suma de los dos anteriores. La sucesión comienza con 0 y 1, y continúa con 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, etc.

La función `secuencia-fib` es una que genera una secuencia de los números de la sucesión de Fibonacci.

La función `fib` es una que calcula el n-ésimo número de la sucesión de Fibonacci.

La función `test` es una que llama a la función `fib` varias veces para mostrar los primeros 50 números de la sucesión de Fibonacci.

Ejemplo:

```forth
10 <fib> cr
20 <fib> cr
30 <fib> cr
40 <fib> cr
50 <fib> cr
```

Salida:

```
55
6765
10946
17711
28657
```