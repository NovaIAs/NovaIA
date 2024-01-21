```f#
// Crear un tipo de datos para representar un árbol binario de búsqueda
type ArbolBinario<'a> =
    | Hoja
    | Nodo of 'a * ArbolBinario<'a> * ArbolBinario<'a>

// Crear un árbol binario de búsqueda a partir de una lista de elementos
let crearArbolBinario<'a>(lista: 'a list): ArbolBinario<'a> =
    match lista with
    | [] -> Hoja
    | x::xs -> Nodo(x, crearArbolBinario xs, crearArbolBinario(List.tail xs))

// Buscar un elemento en un árbol binario de búsqueda
let buscar<'a>(elemento: 'a) (arbol: ArbolBinario<'a>): bool =
    match arbol with
    | Hoja -> false
    | Nodo(x, izquierda, derecha) ->
        if x = elemento then true
        elif elemento < x then buscar elemento izquierda
        else buscar elemento derecha

// Insertar un elemento en un árbol binario de búsqueda
let insertar<'a>(elemento: 'a) (arbol: ArbolBinario<'a>): ArbolBinario<'a> =
    match arbol with
    | Hoja -> Nodo(elemento, Hoja, Hoja)
    | Nodo(x, izquierda, derecha) ->
        if elemento < x then Nodo(x, insertar elemento izquierda, derecha)
        else Nodo(x, izquierda, insertar elemento derecha)

// Eliminar un elemento de un árbol binario de búsqueda
let eliminar<'a>(elemento: 'a) (arbol: ArbolBinario<'a>): ArbolBinario<'a> =
    match arbol with
    | Hoja -> Hoja
    | Nodo(x, izquierda, derecha) ->
        if elemento < x then Nodo(x, eliminar elemento izquierda, derecha)
        elif elemento > x then Nodo(x, izquierda, eliminar elemento derecha)
        else
            match izquierda, derecha with
            | Hoja, Hoja -> Hoja
            | Hoja, _ -> derecha
            | _, Hoja -> izquierda
            | _, _ ->
                let minDerecha = encontrarMinimo derecha in
                Nodo(minDerecha, izquierda, eliminar minDerecha derecha)

// Encontrar el elemento mínimo en un árbol binario de búsqueda
let encontrarMinimo<'a>(arbol: ArbolBinario<'a>): 'a =
    match arbol with
    | Hoja -> failwith "El árbol está vacío"
    | Nodo(x, izquierda, derecha) ->
        if izquierda = Hoja then x
        else encontrarMinimo izquierda

// Encontrar el elemento máximo en un árbol binario de búsqueda
let encontrarMaximo<'a>(arbol: ArbolBinario<'a>): 'a =
    match arbol with
    | Hoja -> failwith "El árbol está vacío"
    | Nodo(x, izquierda, derecha) ->
        if derecha = Hoja then x
        else encontrarMaximo derecha

// Imprimir un árbol binario de búsqueda en forma de cadena
let imprimirArbol<'a>(arbol: ArbolBinario<'a>): string =
    match arbol with
    | Hoja -> ""
    | Nodo(x, izquierda, derecha) ->
        imprimirArbol izquierda + " " + x.ToString() + " " + imprimirArbol derecha

// Ejemplo de uso
let arbol = crearArbolBinario [10, 5, 15, 2, 7, 12, 20]
let elemento = 12

if buscar elemento arbol then
    printfn "%d está en el árbol" elemento
else
    printfn "%d no está en el árbol" elemento

arbol = insertar 8 arbol
arbol = eliminar 15 arbol

printfn "El árbol después de insertar 8 y eliminar 15 es %s" (imprimirArbol arbol)
```

Explicación del código:

1. **Tipo de datos `ArbolBinario`:** Definimos un tipo de datos personalizado llamado `ArbolBinario` que representa un árbol binario de búsqueda. Un árbol binario de búsqueda es una estructura de datos en la que cada nodo tiene un valor y dos ramas, una rama izquierda y una rama derecha. Los valores en la rama izquierda son menores que el valor del nodo, y los valores en la rama derecha son mayores que el valor del nodo.

2. **Función `crearArbolBinario`:** Esta función toma una lista de elementos y la convierte en un árbol binario de búsqueda. La función utiliza un algoritmo recursivo para construir el árbol.

3. **Función `buscar`:** Esta función toma un elemento y un árbol binario de búsqueda, y devuelve `true` si el elemento está en el árbol, o `false` si no está. La función utiliza un algoritmo recursivo para buscar el elemento en el árbol.

4. **Función `insertar`:** Esta función toma un elemento y un árbol binario de búsqueda, y devuelve un nuevo árbol binario de búsqueda con el elemento insertado. La función utiliza un algoritmo recursivo para insertar el elemento en el árbol.

5. **Función `eliminar`:** Esta función toma un elemento y un árbol binario de búsqueda, y devuelve un nuevo árbol binario de búsqueda con el elemento eliminado. La función utiliza un algoritmo recursivo para eliminar el elemento del árbol.

6. **Función `encontrarMinimo`:** Esta función toma un árbol binario de búsqueda y devuelve el elemento mínimo del árbol. La función utiliza un algoritmo recursivo para encontrar el elemento mínimo del árbol.

7. **Función `encontrarMaximo`:** Esta función toma un árbol binario de búsqueda y devuelve el elemento máximo del árbol. La función utiliza un algoritmo recursivo para encontrar el elemento máximo del árbol.

8. **Función `imprimirArbol`:** Esta función toma un árbol binario de búsqueda y devuelve una cadena que representa el árbol. La función utiliza un algoritmo recursivo para imprimir el árbol en forma de cadena.

9. **Ejemplo de uso:** En la parte inferior del código, se crea un árbol binario de búsqueda a partir de una lista de números, y luego se insertan y eliminan elementos del árbol. También se imprimen el árbol antes y después de insertar y eliminar elementos.

Este código es un ejemplo de cómo se puede utilizar un árbol binario de búsqueda en F#. Los árboles binarios de búsqueda son estructuras de datos muy útiles para almacenar y buscar datos de manera eficiente.