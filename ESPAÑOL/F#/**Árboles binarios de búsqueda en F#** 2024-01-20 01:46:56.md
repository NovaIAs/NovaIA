```f#

// Este código implementa un árbol binario de búsqueda en F#.

// Definimos el tipo de datos para los nodos del árbol.
type Nodo<'a> =
    | Hoja
    | Rama of 'a * Nodo<'a> * Nodo<'a>

// Definimos el tipo de datos para el árbol binario de búsqueda.
type Arbol<'a> =
    | Vacio
    | NodoBinario of Nodo<'a>

// Función que inserta un elemento en el árbol binario de búsqueda.
let insertar<'a> (x: 'a) (t: Arbol<'a>) : Arbol<'a> =
    match t with
    | Vacio -> NodoBinario (Nodo (x, Hoja, Hoja))
    | NodoBinario (n) ->
        if x < n.data then NodoBinario (insertar x n.izquierda)
        else NodoBinario (insertar x n.derecha)

// Función que busca un elemento en el árbol binario de búsqueda.
let buscar<'a> (x: 'a) (t: Arbol<'a>) : bool =
    let rec buscarAux (n: Nodo<'a>) : bool =
        match n with
        | Hoja -> false
        | Rama (_, izquierda, derecha) ->
            if x = n.data then true
            elif x < n.data then buscarAux izquierda
            else buscarAux derecha
    match t with
    | Vacio -> false
    | NodoBinario (n) -> buscarAux n

// Función que elimina un elemento del árbol binario de búsqueda.
let eliminar<'a> (x: 'a) (t: Arbol<'a>) : Arbol<'a> =
    let rec eliminarAux (n: Nodo<'a>) : Nodo<'a> =
        match n with
        | Hoja -> Hoja
        | Rama (_, izquierda, derecha) ->
            if x = n.data then
                match izquierda, derecha with
                | _, Hoja -> izquierda
                | Hoja, _ -> derecha
                | _, _ ->
                    let y = encontrarMayor izquierda in
                    Rama (y, eliminarAux izquierda, derecha)
            elif x < n.data then Rama (n.data, eliminarAux izquierda, n.derecha)
            else Rama (n.data, n.izquierda, eliminarAux derecha)
    match t with
    | Vacio -> Vacio
    | NodoBinario (n) -> NodoBinario (eliminarAux n)

// Función que encuentra el elemento mayor en un árbol binario de búsqueda.
let encontrarMayor<'a> (t: Arbol<'a>) : 'a =
    let rec encontrarMayorAux (n: Nodo<'a>) : 'a =
        match n with
        | Hoja -> failwith "No se puede encontrar el elemento mayor en un árbol vacío"
        | Rama (_, _, Hoja) -> n.data
        | Rama (_, _, derecha) -> encontrarMayorAux derecha
    match t with
    | Vacio -> failwith "No se puede encontrar el elemento mayor en un árbol vacío"
    | NodoBinario (n) -> encontrarMayorAux n

// Función que imprime el árbol binario de búsqueda en preorden.
let imprimirPreorden<'a> (t: Arbol<'a>) : unit =
    let rec imprimirPreordenAux (n: Nodo<'a>) : unit =
        match n with
        | Hoja -> ()
        | Rama (x, izquierda, derecha) ->
            printf "%d " x
            imprimirPreordenAux izquierda
            imprimirPreordenAux derecha
    match t with
    | Vacio -> ()
    | NodoBinario (n) -> imprimirPreordenAux n

// Función que imprime el árbol binario de búsqueda en inorden.
let imprimirInorden<'a> (t: Arbol<'a>) : unit =
    let rec imprimirInordenAux (n: Nodo<'a>) : unit =
        match n with
        | Hoja -> ()
        | Rama (x, izquierda, derecha) ->
            imprimirInordenAux izquierda
            printf "%d " x
            imprimirInordenAux derecha
    match t with
    | Vacio -> ()
    | NodoBinario (n) -> imprimirInordenAux n

// Función que imprime el árbol binario de búsqueda en postorden.
let imprimirPostorden<'a> (t: Arbol<'a>) : unit =
    let rec imprimirPostordenAux (n: Nodo<'a>) : unit =
        match n with
        | Hoja -> ()
        | Rama (x, izquierda, derecha) ->
            imprimirPostordenAux izquierda
            imprimirPostordenAux derecha
            printf "%d " x
    match t with
    | Vacio -> ()
    | NodoBinario (n) -> imprimirPostordenAux n

// Función que imprime el árbol binario de búsqueda en forma gráfica.
let imprimirGrafico<'a> (t: Arbol<'a>) : unit =
    let rec imprimirGraficoAux (n: Nodo<'a>) (nivel: int) : unit =
        match n with
        | Hoja -> ()
        | Rama (x, izquierda, derecha) ->
            printf "%d (%d)\n" x nivel
            imprimirGraficoAux izquierda (nivel + 1)
            imprimirGraficoAux derecha (nivel + 1)
    match t with
    | Vacio -> ()
    | NodoBinario (n) -> imprimirGraficoAux n 0

```

Este código implementa un árbol binario de búsqueda en F#. Un árbol binario de búsqueda es una estructura de datos que almacena elementos en un orden específico.

El código comienza definiendo el tipo de datos para los nodos del árbol. Un nodo puede ser una hoja (es decir, no tiene hijos) o una rama (es decir, tiene un hijo izquierdo y un hijo derecho).

A continuación, se define el tipo de datos para el árbol binario de búsqueda. Un árbol puede ser vacío o un nodo binario, que es un nodo con un hijo izquierdo y un hijo derecho.

El código también define una serie de funciones para trabajar con árboles binarios de búsqueda. Estas funciones incluyen la función insertar, que inserta un elemento en el árbol, la función buscar, que busca un elemento en el árbol, la función eliminar, que elimina un elemento del árbol, la función encontrarMayor, que encuentra el elemento mayor en el árbol, la función imprimirPreorden, que imprime el árbol en preorden, la función imprimirInorden, que imprime el árbol en inorden, la función imprimirPostorden, que imprime el árbol en postorden, y la función imprimirGrafico, que imprime el árbol en forma gráfica.

Este código es un ejemplo de cómo se puede implementar un árbol binario de búsqueda en F#.