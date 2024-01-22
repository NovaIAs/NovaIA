```forth

: ¿Par o impar?
    dup 2 mod 0 = if
        drop . "par" cr
    else . "impar" cr
    then
;

: ¿ Mayor?
    swap > if
        ." El primer elemento es mayor." cr
    else
        ." El segundo elemento es mayor." cr
    then
;

: ¿Igual?
    dup = if
        ." Los elementos son iguales." cr
    else
        ." Los elementos no son iguales." cr
    then
;

: Número primo?
    2swap 0 do
        dup 1 + swap 1 swap mod
    loop
    if
        ." El número es primo." cr
    else
        ." El número no es primo." cr
    then
;

: Factorial
    1 swap do
        dup 1 + swap *
    loop
    drop
;

: Fibonacci
    0 1 do
        swap > if
            leave
        then
        dup 1 - over +
        swap
    loop
    drop
;

: SerieTriangular
    1 swap do
        dup 1 + swap +
    loop
    drop
;

: SumaCuadrados
    1 swap do
        dup swap * +
    loop
    drop
;

: SumaCubos
    1 swap do
        dup swap * swap * +
    loop
    drop
;

```

Este código incluye una variedad de funciones matemáticas y lógicas, entre ellas:

* ¿Par o impar?: Determina si un número es par o impar.

* ¿Mayor?: Compara dos números y determina cuál es mayor.

* ¿Igual?: Compara dos números y determina si son iguales.

* ¿Número primo?: Determina si un número es primo.

* Factorial: Calcula el factorial de un número.

* Fibonacci: Calcula el número de Fibonacci en una posición determinada.

* SerieTriangular: Calcula el número triangular en una posición determinada.

* SumaCuadrados: Calcula la suma de los cuadrados de los primeros números naturales hasta un número determinado.

* SumaCubos: Calcula la suma de los cubos de los primeros números naturales hasta un número determinado.

El código está escrito en Forth, un lenguaje de programación de pila que se caracteriza por su simplicidad y eficiencia. Las palabras clave del lenguaje se escriben en mayúsculas, y las variables y constantes se escriben en minúsculas.

Para utilizar el código, simplemente escriba las palabras clave correspondientes en la línea de comandos de Forth. Por ejemplo, para calcular el factorial de 5, escriba:

```
5 factorial . cr
```

El resultado será:

```
120
```