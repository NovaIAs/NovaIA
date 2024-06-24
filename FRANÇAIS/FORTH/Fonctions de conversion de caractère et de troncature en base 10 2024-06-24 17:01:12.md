```forth
: soustracteur ( a b -- a-b )
    nip swap - ;

: chiffre? ( u -- bool )
    128 + 65 < and 128 - 90 < or ;

: minuscule? ( c -- bool )
    65 - 90 + < ;

: chiffre? ( u -- bool )
    128 + 65 < and 128 - 90 < or ;

: minuscule? ( c -- bool )
    65 - 90 + < ;

: minuscule? ( c -- bool )
    65 - 90 + < ;

: caractere? ( c -- bool )
    chiffre? or minuscule? ;

: tronque ( n -- n' )
    100 mod ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10 * over +
    then ;

: base10 ( u -- n )
    u chiffre? if
        u 48 -
        swap 10