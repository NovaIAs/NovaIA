```forth
: CrearPila ( cantidad -- pila )
    Dup 0 Do
        Swap I
    Loop Drop
;

: EliminarPila ( pila -- )
    Begin Dup While
        Swap Drop
    Repeat Drop
;

: CopiarPila ( pila pila -- pila )
    CrearPila Swap C@ +
    Begin
        Swap Over @ Dup C!
    Until Drop Swap Drop
;

: ImprimirPila ( pila -- )
    Begin Dup While
        C@ SPACES .
    Repeat Drop
    CR
;

: IngresarElemento ( elemento pila -- pila )
    Swap C!
;

: ExtraerElemento ( pila -- elemento )
    C@
;

: PilaLlena? ( pila -- bandera )
    C! =
;

: PilaVacia? ( pila -- bandera )
    0 =
;

: SumarPila ( pila -- suma )
    0 Swap
    Begin
        Swap C@ +
    Until Drop
;

: RestarPila ( pila -- diferencia )
    0 Swap
    Begin
        Swap C@ -
    Until Drop
;

: MultiplicarPila ( pila -- producto )
    1 Swap
    Begin
        Swap C@ *
    Until Drop
;

: DividirPila ( pila -- cociente )
    1 Swap
    Begin
        Swap C@ /
    Until Drop
;

: PotenciaPila ( pila -- potencia )
    1 Swap
    Begin
        Swap C@ ^
    Until Drop
;

: FactorialPila ( pila -- factorial )
    0 C! 1 = While
        1 + SWAP
    Repeat
    C@
;

: FibonacciPila ( pila -- fibonacci )
    1 C! 1 C!
    Begin
        Swap C@ + C@
        2 UnderSwap 1 Over +
    Until Drop
;

: PalindromoPila ( pila -- bandera )
    CrearPila Swap C@ +
    Begin
        Dup C@ Over C@ = If
            Leave
        Else
            Swap C!
            Drop
        Then
    Until Drop
    Swap EliminarPila
;

: OrdenarPila ( pila -- pilaOrdenada )
    Begin
        PilaVacia? While
            CrearPila Swap
            Dup PilaVacia? Until
                ExtraerElemento IngresarElemento
            Drop
            CopiarPila Swap Intercambiar
            EliminarPila
        Repeat
    Drop
;

: BuscarElementoPila ( pila elemento -- indice )
    Dup C@ 0 Do
        Over @ = While 1 + Repeat
    Loop Drop
;

: EliminarIndicePila ( pila indice -- pila )
    Dup C@ 0 Do
        Over @ indice = While
            1 - Replace
        Repeat
    Loop Drop
;

: InsertarIndicePila ( pila indice elemento -- pila )
    Dup C@ 0 Do
        Over @ indice >= While
            Swap 1 + Replace
        Repeat
    Loop Over IngresarElemento
;

: ImprimirLista ( lista -- )
    Begin Dup While
        C@ SPACES .
    Repeat Drop
    CR
;

: CrearLista ( cantidad -- lista )
    Dup 0 Do
        Swap @ CrearPila
    Loop Drop
;

: EliminarLista ( lista -- )
    Begin Dup While
        Swap EliminarPila
    Repeat Drop
;

: CopiarLista ( lista lista -- lista )
    CrearLista Swap C@ +
    Begin
        Swap Over @ Dup C!
    Until Drop Swap Drop
;

: IngresarElementoLista ( elemento lista -- lista )
    Swap C!
;

: ExtraerElementoLista ( lista -- elemento )
    C@
;

: ListaLlena? ( lista -- bandera )
    C! =
;

: ListaVacia? ( lista -- bandera )
    0 =
;

: SumarLista ( lista -- suma )
    0 Swap
    Begin
        Swap C@ +
    Until Drop
;

: RestarLista ( lista -- diferencia )
    0 Swap
    Begin
        Swap C@ -
    Until Drop
;

: MultiplicarLista ( lista -- producto )
    1 Swap
    Begin
        Swap C@ *
    Until Drop
;

: DividirLista ( lista -- cociente )
    1 Swap
    Begin
        Swap C@ /
    Until Drop
;

: PotenciaLista ( lista -- potencia )
    1 Swap
    Begin
        Swap C@ ^
    Until Drop
;

: FactorialLista ( lista -- factorial )
    0 C! 1 = While
        1 + SWAP
    Repeat
    C@
;

: FibonacciLista ( lista -- fibonacci )
    1 C! 1 C!
    Begin
        Swap C@ + C@
        2 UnderSwap 1 Over +
    Until Drop
;

: PalindromoLista ( lista -- bandera )
    CrearLista Swap C@ +
    Begin
        Dup C@ Over C@ = If
            Leave
        Else
            Swap C!
            Drop
        Then
    Until Drop
    Swap EliminarLista
;

: OrdenarLista ( lista -- listaOrdenada )
    Begin
        ListaVacia? While
            CrearLista Swap
            Dup ListaVacia? Until
                ExtraerElementoLista IngresarElementoLista
            Drop
            CopiarLista Swap Intercambiar
            EliminarLista
        Repeat
    Drop
;

: BuscarElementoLista ( lista elemento -- indice )
    Dup C@ 0 Do
        Over @ = While 1 + Repeat
    Loop Drop
;

: EliminarIndiceLista ( lista indice -- lista )
    Dup C@ 0 Do
        Over @ indice = While
            1 - Replace
        Repeat
    Loop Drop
;

: InsertarIndiceLista ( lista indice elemento -- lista )
    Dup C@ 0 Do
        Over @ indice >= While
            Swap 1 + Replace
        Repeat
    Loop Over IngresarElementoLista
;

: DefinirDiccionario ( nombre -- direccion )
    ": " $ Intercambiar VOcabuLario C!
;

: UsarDiccionario ( nombre -- )
    VOcabuLario C@
;

: CrearDiccionario ( nombre -- direccion )

    DefinirDiccionario nombre
    CargarDiccionario nombre UsaRdiccionario

;

: CargarDiccionario ( nombre -- )

    Dup Intercambiar DefineDiccionario
    "" = If
        CrearDiccionario nombre
    Then

;

: Pila-ValorDoble ( pila -- pila doble )
    Dup SWAP SWAP
;

: Eliminar-Ultimo ( pila -- pila )
    Dup C@ 1 -
;

: Pila-Vacia? ( pila -- bandera )
    C@ 0 =
;

: Intercambiar-Elementos ( pila pila -- pila pila )
    Swap
;

: Duplicar-Ultimo ( pila -- pila pila )
    Dup C@
;

: Sumar-Valores ( pila pila -- pila )
    +
;

: Restar-Valores ( pila pila -- pila )
    -
;

: Multiplicar-Valores ( pila pila -- pila )
    *
;

: Dividir-Valores ( pila pila -- pila )
    /
;

: Potencia-Valores ( pila pila -- pila )
    ^
;

: Negar-Valor ( pila -- pila )
    -1 *
;

: Duplicar-Todos ( pila -- pila pila )
    Begin
        Pila-Vacia?
        While
            Duplicar-Ultimo
        Repeat
    Drop
;

: Intercambiar-Todos ( pila pila -- pila pila )
    Begin
        Pila-Vacia?
        While
            Intercambiar-Elementos
        Repeat
    Drop
;

: Sumar-Todos ( pila -- pila )
    Begin
        Pila-Vacia?
        While
            Sumar-Valores
        Repeat
    Drop
;

: Restar-Todos ( pila -- pila )
    Begin
        Pila-Vacia?
        While
            Restar-Valores
        Repeat
    Drop
;

: Multiplicar-Todos ( pila -- pila )
    Begin
        Pila-Vacia?
        While
            Multiplicar-Valores
        Repeat
    Drop
;

: Dividir-Todos ( pila -- pila )
    Begin
        Pila-Vacia?
        While
            Dividir-Valores
        Repeat
    Drop
;

: Potencia-Todos ( pila -- pila )
    Begin
        Pila-Vacia?
        While
            Potencia-Valores
        Repeat
    Drop
;

: Calcular-Factorial ( pila -- pila )
    Begin
        Pila-Vacia?
        While
            Duplicar-Ultimo
            1 -
            Multiplicar-Valores
        Repeat
    Drop
;

: Calcular-Fibonacci ( pila -- pila )
    Begin
        Pila-Vacia?
        While
            Duplicar-Ultimo
            Eliminar-Ultimo
            Duplicar-Ultimo
            Eliminar-Ultimo
            Sumar-Valores
        Repeat
    Drop
;

: Comprobar-Palindromo ( pila -- bandera )
    Intercambiar-Todos
    Igualar-Valores
;

: Ordenar-Pila ( pila -- pila ordenada )
    Begin
        Pila-Vacia?
        While
            Crear-Pila-Auxiliar
            Copiar-Pila-Auxiliar
            Eliminar-Ultimo
            Ordenar-Pila-Auxiliar
            Copiar-Pila-Principal
        Repeat
    Drop
;

: Buscar-Elemento-Pila ( pila elemento -- indice )
    Begin
        Pila-Vacia?
        While
            Duplicar-Ultimo
            Elemento-Igual?
            Igualar-Valores
        Repeat
    Drop
;