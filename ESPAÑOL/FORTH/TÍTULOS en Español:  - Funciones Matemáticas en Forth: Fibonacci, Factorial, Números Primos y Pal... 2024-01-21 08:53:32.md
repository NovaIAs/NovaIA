```forth
: Fibonacci ( n -- f )
    dup 0=
    if 0
    else
        dup 1=
        if 1
        else
            swap 1- Fibonacci +
            swap Fibonacci +
        then
    then ;

: Factorial ( n -- f )
    dup 0=
    if 1
    else dup 1- Factorial *
    then ;

: Primos ( n -- )
    1
    loop
        dup 0=
        if exit
        else
            dup 2=
            if
                print cr
                drop
            else
                begin
                    i 3 2 *
                    i 2 dup mod 0=
                    while 2drop
                    repeat
                again
                if dup print cr drop
                else drop
                then
            then
        then
    repeat ;

: Palindromo ( str -- bool )
    str 0 swap str length 2 /
    2swap
    begin
        dup 0= or swap @ @ =
        while 2dup =
        repeat
        drop
    again ;

: Torre-Hanoi ( n discos origen destino auxiliar -- )
    dup 0=
    if drop exit
    else
        dup 1=
        if drop
            [origen destino] Torre-Hanoi
            [origen auxiliar] Torre-Hanoi
            [auxiliar destino] Torre-Hanoi
        else
            [origen auxiliar] Torre-Hanoi
            [origen destino] Torre-Hanoi
            [auxiliar destino] Torre-Hanoi
        then
    then ;

: Busca-Binaria ( lista valor -- pos )
    0 swap
    lista length 1- swap
    while
        2dup @ < <>
        while 2drop 1+
        repeat
        2drop i 2 /
        swap @ if leave then
        swap < if 2drop swap then
        dup +
    repeat ;
```