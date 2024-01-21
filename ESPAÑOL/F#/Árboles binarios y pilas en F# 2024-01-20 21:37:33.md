```f#

// Definición del tipo de datos para la pila
type Stack<'a> =
    | Empty
    | Push of 'a * Stack<'a>

// Definición del tipo de datos para el árbol binario
type BinaryTree<'a> =
    | Leaf of 'a
    | Node of 'a * BinaryTree<'a> * BinaryTree<'a>

// Función para crear una pila vacía
let emptyStack<'a> () : Stack<'a> =
    Empty

// Función para insertar un elemento en una pila
let push<'a> (x: 'a) (s: Stack<'a>) : Stack<'a> =
    Push(x, s)

// Función para retirar un elemento de una pila
let pop<'a> (s: Stack<'a>) : 'a * Stack<'a> =
    match s with
    | Empty -> failwith "Pila vacía"
    | Push(x, xs) -> (x, xs)

// Función para crear un árbol binario vacío
let emptyTree<'a> () : BinaryTree<'a> =
    Leaf null

// Función para insertar un elemento en un árbol binario
let insert<'a> (x: 'a) (t: BinaryTree<'a>) : BinaryTree<'a> =
    match t with
    | Leaf null -> Leaf x
    | Leaf y -> Node(y, Leaf x, Leaf null)
    | Node(y, l, r) -> Node(y, insert x l, insert x r)

// Función para buscar un elemento en un árbol binario
let find<'a> (x: 'a) (t: BinaryTree<'a>) : bool =
    match t with
    | Leaf null -> false
    | Leaf y -> x = y
    | Node(y, l, r) -> x = y || find x l || find x r

// Función para eliminar un elemento de un árbol binario
let delete<'a> (x: 'a) (t: BinaryTree<'a>) : BinaryTree<'a> =
    match t with
    | Leaf null -> Leaf null
    | Leaf y -> if x = y then Leaf null else Leaf y
    | Node(y, l, r) ->
        if x = y then
            match l, r with
            | Leaf null, Leaf null -> Leaf null
            | _, Leaf null -> l
            | Leaf null, _ -> r
            | _, _ -> let min = findMin r in Node(min, l, delete min r)
        else Node(y, delete x l, delete x r)

// Función para encontrar el elemento mínimo en un árbol binario
let findMin<'a> (t: BinaryTree<'a>) : 'a =
    match t with
    | Leaf x -> x
    | Node(x, l, _) -> findMin l

// Función para encontrar el elemento máximo en un árbol binario
let findMax<'a> (t: BinaryTree<'a>) : 'a =
    match t with
    | Leaf x -> x
    | Node(x, _, r) -> findMax r

// Función para imprimir un árbol binario en preorden
let printPreorder<'a> (t: BinaryTree<'a>) : unit =
    match t with
    | Leaf x -> printfn "%d " x
    | Node(x, l, r) ->
        printfn "%d " x
        printPreorder l
        printPreorder r

// Función para imprimir un árbol binario en inorden
let printInorder<'a> (t: BinaryTree<'a>) : unit =
    match t with
    | Leaf x -> printfn "%d " x
    | Node(x, l, r) ->
        printInorder l
        printfn "%d " x
        printInorder r

// Función para imprimir un árbol binario en postorden
let printPostorder<'a> (t: BinaryTree<'a>) : unit =
    match t with
    | Leaf x -> printfn "%d " x
    | Node(x, l, r) ->
        printPostorder l
        printPostorder r
        printfn "%d " x

// Función para probar las funciones definidas anteriormente
let main () : unit =
    let s = emptyStack()
    let s' = push 1 s
    let s'' = push 2 s'
    printfn "Pila: %A" s''
    let x, s''' = pop s''
    printfn "Elemento retirado de la pila: %d" x
    printfn "Pila: %A" s'''
    let t = emptyTree()
    let t' = insert 1 t
    let t'' = insert 2 t'
    let t''' = insert 3 t''
    printfn "Árbol binario:"
    printPreorder t'''
    printfn ""
    printInorder t'''
    printfn ""
    printPostorder t'''
    printfn ""
    if find 2 t''' then printfn "El elemento 2 está en el árbol"
    else printfn "El elemento 2 no está en el árbol"
    let t'''' = delete 2 t'''
    printfn "Árbol binario después de eliminar el elemento 2:"
    printPreorder t''''
    printfn ""
    printInorder t''''
    printfn ""
    printPostorder t''''
    printfn ""
    printfn "Elemento mínimo en el árbol: %d" (findMin t''''))
    printfn "Elemento máximo en el árbol: %d" (findMax t''''))

// Punto de entrada al programa
main()

```

Este código es una implementación de una pila y un árbol binario en F#. La pila se implementa utilizando una lista enlazada, mientras que el árbol binario se implementa utilizando una estructura de datos recursiva. El código también incluye funciones para insertar, eliminar, buscar y recorrer tanto la pila como el árbol binario.

Para probar el código, puedes ejecutar el siguiente comando en una terminal:

```
fsc .\binary-tree-and-stack.fs
```

Esto compilará el código y creará un ejecutable llamado `binary-tree-and-stack.exe`. Puedes ejecutar este ejecutable para ver la salida del programa.