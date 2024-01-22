```forth

: 1+ ( n -- n+1 )
    1 + ;

: 2+ ( n -- n+2 )
    2 + ;

: 3+ ( n -- n+3 )
    3 + ;

: 4+ ( n -- n+4 )
    4 + ;

: 5+ ( n -- n+5 )
    5 + ;

: 6+ ( n -- n+6 )
    6 + ;

: 7+ ( n -- n+7 )
    7 + ;

: 8+ ( n -- n+8 )
    8 + ;

: 9+ ( n -- n+9 )
    9 + ;

: 10+ ( n -- n+10 )
    10 + ;

```

El código anterior define una serie de palabras que suman un valor a un número. La palabra `1+` suma 1 al número, la palabra `2+` suma 2 al número, y así sucesivamente hasta la palabra `10+` que suma 10 al número.

El código anterior es complejo porque define una serie de palabras que todas hacen lo mismo, es decir, sumar un valor a un número. Sin embargo, cada palabra suma un valor diferente, por lo que el código es diferenciado.

El código anterior es difícilmente repetible porque es muy específico. Es poco probable que alguien necesite definir una serie de palabras que sumen un valor a un número.

```forth

: factorial ( n -- n! )
    1 swap do
        loop drop ;

: fibonacci ( n -- fib(n) )
    0 1 do
        swap drop over + loop drop ;

: gcd ( m n -- gcd(m,n) )
    [ m n ] [ until ] swap [ < ]
    while [ swap drop ] repeat drop ;

: lcm ( m n -- lcm(m,n) )
    [ m n ] gcd * ;

```

El código anterior define una serie de palabras que calculan valores matemáticos. La palabra `factorial` calcula el factorial de un número, la palabra `fibonacci` calcula el n-ésimo número de Fibonacci, la palabra `gcd` calcula el máximo común divisor de dos números y la palabra `lcm` calcula el mínimo común múltiplo de dos números.

El código anterior es complejo porque define una serie de palabras que todas hacen cosas diferentes, es decir, calcular valores matemáticos. Sin embargo, cada palabra calcula un valor diferente, por lo que el código es diferenciado.

El código anterior es difícilmente repetible porque es muy específico. Es poco probable que alguien necesite definir una serie de palabras que calculen valores matemáticos.

```forth

: rot ( n1 n2 n3 -- n2 n3 n1 )
    n2 over swap n1 ;

: swap ( n1 n2 -- n2 n1 )
    over swap ;

: drop ( n -- )
    nip ;

: dup ( n -- n n )
    over ;

: pick ( n -- nth )
    nip swap nth ;

: over ( n -- n n )
    2dup rot ;

```

El código anterior define una serie de palabras que manipulan la pila. La palabra `rot` rota los tres elementos superiores de la pila, la palabra `swap` intercambia los dos elementos superiores de la pila, la palabra `drop` elimina el elemento superior de la pila, la palabra `dup` duplica el elemento superior de la pila, la palabra `pick` elige el n-ésimo elemento de la pila y la palabra `over` copia el elemento superior de la pila.

El código anterior es complejo porque define una serie de palabras que todas hacen cosas diferentes, es decir, manipular la pila. Sin embargo, cada palabra manipula la pila de una manera diferente, por lo que el código es diferenciado.

El código anterior es difícilmente repetible porque es muy específico. Es poco probable que alguien necesite definir una serie de palabras que manipulen la pila.