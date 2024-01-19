```forth
: crear-arreglo ( tipo tam )
    malloc tam tipo
;


: eliminar-arreglo ( arreglo )
    free arreglo
;


: obtener-elemento ( arreglo indice )
    @ +
;


: establecer-elemento ( arreglo elemento indice )
    dup @ + !
    drop
;


: imprimir-arreglo ( arreglo tam )
    0 tam do
        dup @ .
        1 + loop
    drop
;


: ordenar-arreglo ( arreglo tam )
    0 tam 1 - do
        dup @ + 1 + dup @ > if
            dup @ dup @ + 1 + swap !
        else
            drop
        then
    loop
    drop
;

                                                                                                                                                                                                                                  
: obtener-lista ( tam )
    crear-arreglo celula tam
    dup 0 0 do
        i @ celula @ !
    loop
    drop
;


: imprimir-lista ( lista tam )
    rot 0 tam do
        dup @
        dup celula @
        @ .
        1 + loop
    drop
    drop
;


: crear-nodo ( valor )
    crear-arreglo celula 2
    dup 0 valor !
    dup 1 0 !
    drop
;


: imprimir-nodo ( nodo )
    rot @ celula @
    dup 0 @ .
    dup 1 @ .
    drop
    drop
;

                                                                                                                                                                                                                                      
: crear-cola ( tam )
    crear-arreglo celula tam
    dup 0 0 !
    dup 1 tam 1 - !
    drop
;


: insertar-cola ( cola elemento )
    dup 1 @ @ dup @ + 1 mod !
    dup 0 @ @ elemento !
    1 + !
;


: quitar-cola ( cola )
    dup 0 @ @
    dup 1 @ @ 1 - mod !
    1 - !
;


: imprimir-cola ( cola )
    dup 0 @ dup @ do
        dup @ celula @
        @ .
    loop
    drop
    drop
;
```