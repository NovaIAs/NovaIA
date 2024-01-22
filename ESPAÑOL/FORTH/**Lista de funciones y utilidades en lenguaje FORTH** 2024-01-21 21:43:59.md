```forth
: saludar ( -- )
    . "Hola, mundo!" cr
;

: saludar-a ( nombre -- )
    "Hola, " nombre . cr
;

: promedio ( n1 n2 -- promedio )
    + 2 /
;

: mayor-de ( n1 n2 -- mayor )
    > if
        n1
    else
        n2
    then
;

: fibonacci ( n -- resultado )
    0 1 2dup  while
        swap +
        tuck
    repeat
    drop
;

: factorial ( n -- factorial )
    0 > if
        1
    else
        dup 1 - factorial *
    then
;

: es-primo ( n -- flag )
    2 2 > while
        n mod 0 = if
            false
        then
        dup 1 + 2dup  > swap  while
            n mod 0 = if
                false
            then
            dup 1 +
        repeat
    repeat
    true
;

: primo-siguiente ( n -- p )
    dup 1 + swap  while   [ es-primo ]
    repeat
;

: lista-primos ( n -- nil )
    0 1 2dup  while
        dup es-primo if
            . space
        then
        swap 1 +
    repeat
    drop cr
;

: suma-lista ( lista -- suma )
    0 swap  while
        +
    repeat
    drop
;

: max-lista ( lista -- maximo )
    swap 0  while
        dup > if
            swap
        then
    repeat
    drop
;

: invertir-lista ( lista -- lista invertida )
    nil swap  while
        swap over cons
    repeat
    drop
;

: concatenar-lista ( lista1 lista2 -- lista concatenada )
    swap dup nil = if
        drop
    else
        over swap cons
    then
;

: ordenar-lista ( lista -- lista ordenada )
    swap nil = if
        drop
    else
        dup car max-lista swap drop  swap  invertir-lista
        concatenar-lista swap ordenar-lista
    then
;

: mostrar-lista ( lista -- )
    [ dup nil = if drop else . space then ]  while cr
;

: ordenar-lista-nombres ( lista-nombres -- lista-nombres ordenada )
    [   swap nil = if
            drop
        else
            dup car max-lista swap drop  swap  invertir-lista
            concatenar-lista swap ordenar-lista-nombres
        then
    ]  while
;


: menu-principal ( -- )
    ."Escoge una opción:" cr
    ."1. Saludar" cr
    ."2. Saludar a alguien" cr
    ."3. Calcular el promedio de dos números" cr
    ."4. Obtener el mayor de dos números" cr
    ."5. Calcular el n-ésimo número de Fibonacci" cr
    ."6. Calcular el factorial de un número" cr
    ."7. Comprobar si un número es primo" cr
    ."8. Obtener el siguiente número primo" cr
    ."9. Mostrar una lista de los primeros n números primos" cr
    ."10. Calcular la suma de una lista de números" cr
    ."11. Obtener el máximo de una lista de números" cr
    ."12. Invertir una lista" cr
    ."13. Concatenar dos listas" cr
    ."14. Ordenar una lista de números" cr
    ."15. Ordenar una lista de nombres" cr
    ."16. Salir" cr
    key >r key 48 -
;

: main ( -- )
    menu-principal loop
    ."Adiós!" cr
;
```