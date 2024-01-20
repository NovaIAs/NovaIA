```forth
: CrearLista ( n -- lista )
    Do i Loop
        swap 2dup i @ + @
    Loop Drop ;

: ImprimirLista ( lista -- )
    Do i Loop
        i @ .
    Loop Drop ;

: BuscarElemento ( lista elemento -- i )
    Do i Loop
        i @ = If
            Leave
        Then
    Loop Drop ;

: InsertarElemento ( lista elemento -- lista' )
    dup BuscarElemento If
        Drop
    Else
        2drop >r i @ Over + Swap
        Do i Loop
            r> Over @ Swap @ Over + !
        Loop Drop
        i @ @ r> Over Swap @ Over + !
    Then ;

: EliminarElemento ( lista elemento -- lista' )
    dup BuscarElemento If
        swap 2drop >r i @ Over + Swap
        Do i Loop
            r> Over @ Swap @ Over + !
        Loop Drop
        i @ @ r> Drop
    Else
        Drop
    Then ;

: CrearMatriz ( n m -- matriz )
    dup Do i Loop
        Do j Loop
            0
        Loop
    Loop Drop ;

: ImprimirMatriz ( matriz n m -- )
    Do i Loop
        Do j Loop
            i j @ @ .
        Loop
        Cr
    Loop Drop ;

: BuscarElementoMatriz ( matriz n m elemento -- i j )
    Do i Loop
        Do j Loop
            i j @ @ = If
                Leave
            Then
        Loop
    Loop Drop ;

: InsertarElementoMatriz ( matriz n m elemento -- matriz' )
    dup BuscarElementoMatriz If
        Drop
    Else
        2drop >r i @ j @ Over + Over + Swap
        Do i Loop
            r> Over @ Swap @ Over + !
        Loop Drop
        Do j Loop
            r> Over @ Swap @ Over + !
        Loop Drop
        i j @ @ r> Over Swap @ Over + !
    Then ;

: EliminarElementoMatriz ( matriz n m elemento -- matriz' )
    dup BuscarElementoMatriz If
        swap 2drop >r i @ j @ Over + Over + Swap
        Do i Loop
            r> Over @ Swap @ Over + !
        Loop Drop
        Do j Loop
            r> Over @ Swap @ Over + !
        Loop Drop
        i j @ @ r> Drop
    Else
        Drop
    Then ;
```