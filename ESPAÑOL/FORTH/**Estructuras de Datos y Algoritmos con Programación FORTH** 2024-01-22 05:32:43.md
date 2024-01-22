```forth
: <vector> crear ( n -- v )
    dup 0 crea tabla ;

: <vector> anidar ( n -- v )
    dup 0 fondo tabla ;

: <vector> llenar ( v vtam -- )
    0 do
        i dup @ + swap !
    loop drop ;

: <vector> imprimir ( v vtam -- )
    0 do
        i @ .r
    loop drop ;

: <vector> sumar ( v1 v1tam v2 v2tam -- v3 )
    crear v1tam
    0 do
        i v1 @ v2 @ + swap !
    loop ;

: <vector> mostrar ( v vtam -- )
    crear vtam 2dup llenar
    dup crear 0 <vector> sumar
    dup crear 0 swap llenar
    cr 0 do
        dup @ .r
        dup @ .r 1+ . cr
    loop drop ;

: <mapa> crear ( ktam vtam -- m )
    dup 0 <vector> crear swap 0 crear tabla ;

: <mapa> insertar ( m k v -- )
    m @ swap 2dup crear
    swap 1+ swap 1+
    crear v crear tabla
    swap ! ;

: <mapa> buscar ( m k -- v )
    m @ dup 2dup @ <vector> crear
    1+ swap 1- 0 do
        i @ dup igual si
            drop drop drop 1+ crear !
        entonces
    loop ;

: <mapa> imprimir ( m -- )
    cr
    m @ dup 2dup @ <vector> crear
    dup <vector> imprimir
    dup 0 do
        i @ . cr
    loop
    cr ;

: <mapa> eliminar ( m k -- m )
    m @ dup 2dup @ <vector> crear
    1+ swap 1- 0 do
        i @ dup igual si
            drop drop drop 1+ crear !
        entonces
    loop ;

: <pila> crear ( n -- s )
    dup 0 <vector> crear ;

: <pila> mostrar ( s -- )
    dup @ 2dup @ 0 do
        i @ .r cr
    loop drop ;

: <pila> empujar ( s v -- )
    dup @ 1+ swap crear !
    swap @ 1+ swap ! ;

: <pila> sacar ( s -- v )
    dup @ 2dup @ 1- crear !
    swap @ 1- swap ! ;

: <cola> crear ( n -- q )
    dup 0 <vector> crear ;

: <cola> mostrar ( q -- )
    dup @ 2dup @ 0 do
        i @ .r cr
    loop drop ;

: <cola> encolar ( q v -- )
    dup @ 1+ swap crear !
    swap @ 1+ swap ! ;

: <cola> desencolar ( q -- v )
    dup @ 2dup @ 1- crear !
    swap @ 1- swap ! ;

: <lista> crear -- l
    dup 0 <vector> crear ;

: <lista> mostrar ( l -- )
    dup @ 2dup @ 0 do
        i @ .r cr
    loop drop ;

: <lista> añadir ( l v -- )
    dup @ 1+ swap crear !
    swap @ 1+ swap ! ;

: <lista> eliminar ( l v -- l )
    dup @ 2dup @ 1- crear !
    1+ swap 1- 0 do
        i @ dup @ igual si
            drop drop 1+ crear !
        entonces
    loop ;

: <árbol> crear -- a
    dup 0 crear ;

: <árbol> insertar ( a v -- )
    dup @ 0 <vector> crear
    2dup crear ! swap ! ;

: <árbol> buscar ( a v -- )
    dup @ 2dup @ 0 <vector> crear
    dup @ 0 si
        drop drop 0
    sino
        dup @ 1+ swap < @ 2dup @ <
        dup @ 1- swap < @ 2dup @ <
        si
            drop drop 1 crear 0 !
        entonces
        si
            drop drop 1 crear 1 !
        entonces
    entonces ;

: <árbol> imprimir ( a -- )
    cr
    dup @ 2dup @ 0 <vector> crear
    dup <vector> imprimir
    dup 0 do
        i @ . cr
    loop
    cr ;

: main --
    ( "Hola mundo!" . cr )
    10 <vector> crear 1 5 8 12 9 13 15 19 21 25 llenar
    10 <vector> imprimir cr
    20 <vector> crear 1 4 9 16 25 36 49 64 81 100 llenar
    20 <vector> imprimir cr
    10 <vector> crear 1 2 4 8 16 32 64 128 256 512 llenar
    10 <vector> imprimir cr
    10 <mapa> crear
    "uno" 1 "dos" 2 "tres" 3 "cuatro" 4 "cinco" 5 insertar
    "dos" buscar . cr
    "tres" buscar . cr
    "seis" buscar . cr
    cr
    10 <mapa> imprimir
    10 <mapa> eliminar "uno"
    10 <mapa> imprimir
    10 <mapa> eliminar "dos"
    10 <mapa> imprimir
    10 <mapa> eliminar "tres"
    10 <mapa> imprimir
    10 <mapa> eliminar "cuatro"
    10 <mapa> imprimir
    10 <mapa> eliminar "cinco"
    10 <mapa> imprimir
    10 <pila> crear
    1 2 3 4 5 empujar
    10 <pila> mostrar
    5 sacar . cr
    4 sacar . cr
    10 <pila> mostrar
    10 <cola> crear
    1 2 3 4 5 encolar
    10 <cola> mostrar
    5 desencolar . cr
    4 desencolar . cr
    10 <cola> mostrar
    10 <lista> crear
    1 2 3 4 5 añadir
    10 <lista> mostrar
    3 eliminar
    10 <lista> mostrar
    5 eliminar
    10 <lista> mostrar
    10 <árbol> crear
    100 48 20 50 47 21 49 22 51 46
    insertar
    100 23 52 45
    insertar
    100 19 44
