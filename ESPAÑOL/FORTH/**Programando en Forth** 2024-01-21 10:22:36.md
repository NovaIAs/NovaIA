```forth
: saludar ( -- )
    "hola" . " " nombre @ . cr ;

: fibonacci ( n -- )
    2 < if
        0 else
        1 dup 1 - fibonacci +
    then ;

: menor-que ( a b -- f )
    > if
        false
    else
        true
    then ;

: ordenar ( n a b ... -- a b ... )
    dup 1 - >r
    2drop
    r 0 do
        i+1 over r + swap menor-que
        if
            swap over i over r @ swap r + !
        then
        r +loop ;

: imprimir ( n a b ... -- )
    dup 0 do
        i @ . " "
        i 1 + loop
    drop ;

: fact ( n -- f )
    dup 1 = if
        1
    then
    1 - fact * 0 ;

: is-prime ( n -- f )
    2 fact 1 + > swap 2 /
    2 > while
        dup 1 + swap mod 0 = if
            false exit
        then
    repeat
    true ;

: primos ( n -- )
    1 2 do i . " " loop
    3 1 + 2 while
        is-prime
        if
            i . " "
        then
        2 +
    repeat ;

: main
    13 saludar
    10 fibonacci . cr
    10 23 ordenar imprimir
    100 fact . cr
    100 primos . cr ;
```

Explicación:

* `saludar`: esta palabra imprime un saludo en la consola.
* `fibonacci`: esta palabra calcula el n-ésimo número de Fibonacci usando recursión.
* `menor-que`: esta palabra compara dos números y devuelve `true` si el primer número es menor que el segundo.
* `ordenar`: esta palabra ordena una lista de números en orden ascendente.
* `imprimir`: esta palabra imprime una lista de números en la consola.
* `fact`: esta palabra calcula el factorial de un número.
* `is-prime`: esta palabra comprueba si un número es primo.
* `primos`: esta palabra imprime los números primos hasta un cierto límite.
* `main`: esta palabra llama a las otras palabras para probarlas.