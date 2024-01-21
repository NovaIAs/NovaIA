```f#
// Sección 1: Definición de un árbol binario
type Tree =
    | Hoja(valor: int)
    | Nodo(izquierdo: Tree, valor: int, derecho: Tree)

// Sección 2: Función para insertar un elemento en un árbol
let insertar (valor: int) (árbol: Tree): Tree =
    match árbol with
    | Hoja(_) -> Nodo(Hoja(0), valor, Hoja(0))
    | Nodo(a, v, b) ->
        if valor < v then Nodo(insertar valor a, v, b)
        else Nodo(a, v, insertar valor b)

// Sección 3: Función para buscar un elemento en un árbol
let buscar (valor: int) (árbol: Tree): bool =
    match árbol with
    | Hoja(_) -> false
    | Nodo(a, v, b) ->
        if valor = v then true
        else if valor < v then buscar valor a
        else buscar valor b

// Sección 4: Función para obtener el número de elementos en un árbol
let tamaño (árbol: Tree): int =
    match árbol with
    | Hoja(_) -> 1
    | Nodo(a, _, b) -> tamaño a + tamaño b + 1

// Sección 5: Función para obtener la altura de un árbol
let altura (árbol: Tree): int =
    match árbol with
    | Hoja(_) -> 0
    | Nodo(a, _, b) -> 1 + max (altura a) (altura b)

// Sección 6: Programa principal
let árbol =
    let a = insertar 10 (Hoja(0))
    let b = insertar 5 a
    let c = insertar 15 a
    let d = insertar 2 a
    let e = insertar 7 b
    let f = insertar 12 c
    c

// Sección 7: Impresión del árbol
printfn "Árbol:\n"
imprimir árbol

// Sección 8: Función para imprimir un árbol
let imprimir (árbol: Tree) =
    let rec imprimirAux (stack: Tree list) (nivel: int) =
        match árbol with
        | Hoja(_) ->
            printfn "%s" (String.replicate nivel "  ")
        | Nodo(a, v, b) ->
            imprimirAux [b; a; Hoja(v)] (nivel + 1)
    imprimirAux [árbol] 0

// Sección 9: Impresión de números obtenidos a partir del árbol
printfn "Número de elementos: %d" (tamaño árbol)
printfn "Altura del árbol: %d" (altura árbol)

printfn "Elementos en orden ascendente:"
let rec imprimirListaAsc (lista: int list) =
    match lista with
    | [] -> ()
    | cabeza :: cola ->
        printf "%d " cabeza
        imprimirListaAsc cola
let listaOrdenada =
    let rec obtenerOrdenAsc (árbol: Tree) (lista: int list): int list =
        match árbol with
        | Hoja(valor) -> [valor]
        | Nodo(a, v, b) ->
            obtenerOrdenAsc a (v :: obtenerOrdenAsc b lista)
    obtenerOrdenAsc árbol []
imprimirListaAsc listaOrdenada

printfn "Elementos en orden descendente:"
let listaOrdenadaDesc =
    let rec obtenerOrdenDesc (árbol: Tree) (lista: int list): int list =
        match árbol with
        | Hoja(valor) -> [valor]
        | Nodo(a, v, b) ->
            obtenerOrdenDesc b (v :: obtenerOrdenDesc a lista)
    obtenerOrdenDesc árbol []
imprimirListaAsc listaOrdenadaDesc
```
Explicación del código:

* La sección 1 define el tipo de árbol binario, que puede ser una hoja (con un único valor) o un nodo (con un hijo izquierdo, un valor y un hijo derecho).

* La sección 2 define una función para insertar un valor en un árbol. Si el árbol está vacío, se crea una hoja con el valor insertado. Si el árbol no está vacío, se inserta el valor en el hijo izquierdo o derecho, dependiendo de si el valor es menor o mayor que el valor del nodo actual.

* La sección 3 define una función para buscar un valor en un árbol. Si el árbol está vacío, la función devuelve falso. Si el árbol no está vacío, se busca el valor en el hijo izquierdo o derecho, dependiendo de si el valor es menor o mayor que el valor del nodo actual.

* La sección 4 define una función para obtener el número de elementos en un árbol. Si el árbol está vacío, la función devuelve 1. Si el árbol no está vacío, se suman los elementos del hijo izquierdo y derecho y se añade 1.

* La sección 5 define una función para obtener la altura de un árbol. Si el árbol está vacío, la función devuelve 0. Si el árbol no está vacío, se devuelve 1 más la altura máxima del hijo izquierdo o derecho.

* La sección 6 crea el árbol de ejemplo que se utilizará en el programa principal.

* La sección 7 imprime el árbol utilizando una función auxiliar recursiva que imprime cada nodo y su nivel en el árbol.

* La sección 8 imprime el número de elementos y la altura del árbol.

* La sección 9 imprime los elementos del árbol en orden ascendente y descendente utilizando una función auxiliar recursiva que obtiene los elementos del árbol en el orden deseado.