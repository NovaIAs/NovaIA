```forth
: factorial ( n -- result )
    1 dup 1 = if drop 0 else
    dup 1 - factorial swap * then ;

: es-primo? ( n -- 1 | 0)
    2 dup = if drop 0 else
    2 square > while
        nip dup divmod swap 0 = if drop 0 else
        2* + swap repeat ;

: primeros ( n -- )
    1 2 . 3> while
        dup es-primo? if . then
        2* + swap repeat ;

: fibonacci ( n -- result )
    1 nip = if nip 0 then
    2 nip = if 1 then
    dup swap fibonacci @ swap fibonacci @ + ;

: numeros-fibonacci ( n -- )
    0 . 1 . 2> while
        fibonacci @ drop .
        fibonacci @ dup 2* + fibonacci !
        swap fibonacci @ dup 2* + fibonacci !
        repeat ;
```

Explicación:

* `factorial`: Calcula el factorial de un número.
* `es-primo?`: Devuelve 0 si el número es primo, 1 si no.
* `primeros`: Muestra los primeros n números primos.
* `fibonacci`: Calcula el n-ésimo número de Fibonacci.
* `numeros-fibonacci`: Muestra los primeros n números de Fibonacci.