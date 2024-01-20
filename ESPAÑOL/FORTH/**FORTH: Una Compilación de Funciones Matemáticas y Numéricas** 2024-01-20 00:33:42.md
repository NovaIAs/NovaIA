```forth
: factorial ( n -- n! )
    1 >while
        1 swap *
    repeat ;

: fibonacci ( n -- fib(n) )
    1 = if
        1
    else
        2 = if
            1
        else
            dup 1 - fibonacci @
            dup 2 - fibonacci @ +
        then
    then ;

: es-primo? ( n -- boolean )
    2 >while
        dup mod
        0 = if
            false
        else
            dup sqrt 1 + 1 swap <
        then
    repeat
    drop true ;

: primos ( n -- )
    dup 2 >while
        tuck true >r
        do
            dup mod
            0 = if
                tuck false >r
                exit
            else
                1 +
            then
        loop
    repeat
    drop r>drop ;

: suma-divisores ( n -- suma de los factores )
    1 2dup >r
    do
        dup mod
        0 = if
            r> +
            r>dup * >r
        then
        1 +
    loop
    drop r>drop ;

: perfecto? ( n -- boolean )
    suma-divisores dup = ;

: suma-digitos ( n -- )
    0 >while
        tuck mod 10 +
        10 /
    repeat
    drop ;

: es-armstrong? ( n -- boolean )
    dup suma-digitos dup pow = ;

: capriano? ( n -- boolean )
    0 2dup >r
    do
        dup mod
        0 = if
            drop r>drop false
        else
            tuck es-primo? if
                r> -
            then
        then
        1 +
    loop
    drop true ;

: main
    "5! : " factorial 5 . cr
    "10! : " factorial 10 . cr
    "15! : " factorial 15 . cr

    "Fibonacci(10): " fibonacci 10 . cr
    "Fibonacci(20): " fibonacci 20 . cr
    "Fibonacci(30): " fibonacci 30 . cr

    "23 es primo? : " 23 es-primo? . cr
    "37 es primo? : " 37 es-primo? . cr

    "Los primeros 100 números primos:" cr
    100 primos . cr

    "La suma de los factores de 12: " 12 suma-divisores . cr

    "8128 es perfecto? : " 8128 perfecto? . cr
    "6 es perfecto? : " 6 perfecto? . cr

    "La suma de los dígitos de 12345: " 12345 suma-digitos . cr

    "153 es un número de Armstrong? : " 153 es-armstrong? . cr
    "371 es un número de Armstrong? : " 371 es-armstrong? . cr
    "9474 es un número de Armstrong?" 9474 es-armstrong? . cr

    100 capriano? . cr
;
```

Explicación:

* `factorial`: Calcula el factorial de un número.
* `fibonacci`: Calcula el número de Fibonacci de un índice dado.
* `es-primo?`: Comprueba si un número es primo.
* `primos`: Enumera los primeros `n` números primos.
* `suma-divisores`: Calcula la suma de los factores de un número.
* `perfecto?`: Comprueba si un número es perfecto.
* `suma-digitos`: Calcula la suma de los dígitos de un número.
* `es-armstrong?`: Comprueba si un número es un número de Armstrong.
* `capriano?`: Comprueba si un número es un número de Caprekar.
* `main`: Función principal que llama a las funciones anteriores para demostrar su uso.