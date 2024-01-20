```f#
// Imports
open System
open System.Collections.Generic
open Microsoft.FSharp.Data.TypeProviders
open Microsoft.FSharp.Reflection

// Definición de tipos

// Un tipo para representar un nodo en un árbol binario
type Nodo<'a> =
    | Hoja of 'a
    | Rama of 'a * Nodo<'a> * Nodo<'a>

// Un tipo para representar un árbol binario
type Arbol<'a> = Nodo<'a>

// Funciones de Árbol

// Función para crear un nodo Hoja
let crearHoja (valor: 'a) = Hoja valor

// Función para crear un nodo Rama
let crearRama (valor: 'a) (izq: Nodo<'a>) (der: Nodo<'a>) = Rama (valor, izq, der)

// Función para insertar un valor en un árbol binario
let insertar (valor: 'a) (arbol: Arbol<'a>) =
    match arbol with
    | Hoja _ -> crearHoja valor
    | Rama (v, izq, der) ->
        if valor < v then crearRama v (insertar valor izq) der
        else crearRama v izq (insertar valor der)

// Función para eliminar un valor de un árbol binario
let eliminar (valor: 'a) (arbol: Arbol<'a>) =
    match arbol with
    | Hoja _ -> failwith "El valor no se encuentra en el árbol"
    | Rama (v, izq, der) ->
        if valor < v then crearRama v (eliminar valor izq) der
        else if valor > v then crearRama v izq (eliminar valor der)
        else Rama (encontrarMaximo izq) izq der

// Función para encontrar el valor máximo en un árbol binario
let encontrarMaximo (arbol: Arbol<'a>) =
    match arbol with
    | Hoja v -> v
    | Rama (v, izq, der) -> encontrarMaximo der

// Funciones de Búsqueda

// Función para buscar un valor en un árbol binario
let buscar (valor: 'a) (arbol: Arbol<'a>) =
    match arbol with
    | Hoja _ -> false
    | Rama (v, izq, der) ->
        if valor = v then true
        else if valor < v then buscar valor izq
        else buscar valor der

// Función para encontrar todos los caminos que suman un valor dado en un árbol binario
let encontrarCaminosSuma (valor: int) (arbol: Arbol<int>) =
    let caminos = List()
    let camino = List()
    let encontrarCaminosSumaRec (nodo: Nodo<int>) =
        match nodo with
        | Hoja v ->
            if v = valor then caminos.Add camino
        | Rama (v, izq, der) ->
            camino.Add v
            encontrarCaminosSumaRec izq
            encontrarCaminosSumaRec der
            camino.RemoveAt(camino.Count - 1)
    encontrarCaminosSumaRec arbol
    caminos

// Función para imprimir un árbol binario en forma de cadena
let imprimirArbol (arbol: Arbol<string>) =
    let impresion = StringBuilder()
    let imprimirArbolRec (nodo: Nodo<string>) =
        match nodo with
        | Hoja v -> impresion.AppendLine v
        | Rama (v, izq, der) ->
            impresion.AppendLine v
            impresion.AppendLine "Izquierda:"
            imprimirArbolRec izq
            impresion.AppendLine "Derecha:"
            imprimirArbolRec der
    imprimirArbolRec arbol
    impresion.ToString()

// Uso

// Crear un árbol binario
let arbol =
    crearRama
        10
        (crearRama
            5
            (crearHoja 2)
            (crearHoja 7))
        (crearRama
            15
            (crearHoja 12)
            (crearHoja 20))

// Insertar un valor en el árbol
let arbolConNuevoValor = insertar 8 arbol

// Eliminar un valor del árbol
let arbolSinValor = eliminar 7 arbolConNuevoValor

// Buscar un valor en el árbol
let valorEncontrado = buscar 12 arbolSinValor

// Encontrar todos los caminos que suman un valor dado en el árbol
let caminos = encontrarCaminosSuma 20 arbolSinValor

// Imprimir el árbol
let impresionArbol = imprimirArbol arbolSinValor

// Imprimir los resultados
printfn "Valor encontrado: %b" valorEncontrado
printfn "Caminos que suman 20:"
for camino in caminos do
    let caminoImpreso = camino.Aggregate("", (fun a b -> a + " -> " + b))
    printfn "\t%s" caminoImpreso
printfn "Impresión del árbol:\n%s" impresionArbol
```

Explicación:

* **Tipos:** Se definen los tipos `Nodo` y `Arbol` para representar los nodos y los árboles binarios, respectivamente.
* **Funciones de Árbol:** Se definen funciones para insertar, eliminar, encontrar el valor máximo y buscar un valor en un árbol binario.
* **Funciones de Búsqueda:** Se definen funciones para buscar un valor y encontrar todos los caminos que suman un valor dado en un árbol binario.
* **Funciones de Impresión:** Se define una función para imprimir un árbol binario en forma de cadena.
* **Uso:** Se crea un árbol binario, se inserta un valor, se elimina un valor, se busca un valor, se encuentran todos los caminos que suman un valor dado y se imprime el árbol.