```forth

: factorial  ( n -- n! )
    1 do
        dup @ rot *
    loop drop ;

: fibonacci  ( n -- f(n) )
    dup 0> if
        drop 1
    else
        2dup over 1-call dup over 1+call +
    then ;

: esprimo  ( n -- p1 p2 p3 ... )
    0 do
        dup 2 /mod 0= if
            dup @ . drop
        else
            drop
        then
    loop ;

: potencia  ( m n -- r )
    dup 0= if
        drop 1
    else
        over 2 /mod 0= if
            over 2/mod 0= if
                over potencia over potencia *
            else
                over potencia dup *
            then
        else
            over potencia over potencia * swap *
        then
    then ;

: mcd  ( m n -- r )
    >r swap mod 0= while r >r over mod repeat drop ;

: mcm  ( m n -- r )
    dup swap mcd * ;

: área-círculo  ( r -- a )
    dup potencia pi * ;

: volumen-esfera  ( r -- v )
    dup 4 / 3 potencia pi * ;

: bmi  ( w h -- bmi )
    dup potencia 703 / ;

: edad-perro  ( años-humano -- años-perro )
    dup 16 * 3 agregate + ;

```