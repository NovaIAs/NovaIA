```f#

// Definimos un tipo de datos abstracto para representar un árbol binario.
type 'a arbol =
    | Hoja of 'a
    | Rama of 'a * 'a arbol * 'a arbol

// Definimos una función recursiva para calcular la altura de un árbol.
let altura (arbol : 'a arbol) : int =
    match arbol with
        | Hoja(_) -> 0
        | Rama(_, izq, der) -> 1 + max (altura izq) (altura der)

// Definimos una función recursiva para buscar un elemento en un árbol.
let buscar (elemento : 'a) (arbol : 'a arbol) : bool =
    match arbol with
        | Hoja(valor) -> valor = elemento
        | Rama(valor, izq, der) ->
            if valor = elemento then true
            elif buscar elemento izq then true
            else buscar elemento der

// Definimos una función recursiva para eliminar un elemento de un árbol.
let eliminar (elemento : 'a) (arbol : 'a arbol) : 'a arbol =
    match arbol with
        | Hoja(_) -> raise (InvalidOperation "El elemento no se encuentra en el árbol.")
        | Rama(valor, izq, der) ->
            if valor = elemento then
                // Si el elemento es el nodo raíz, devolvemos el hijo izquierdo o derecho, según corresponda.
                if izq = Hoja(null) then der
                elif der = Hoja(null) then izq
                // Si el elemento no es el nodo raíz, lo eliminamos del hijo izquierdo o derecho, según corresponda.
                else
                    let nuevo_izq = eliminar elemento izq
                    let nuevo_der = eliminar elemento der
                    Rama(valor, nuevo_izq, nuevo_der)
            else
                // Si el elemento no es el nodo raíz, lo eliminamos del hijo izquierdo o derecho, según corresponda.
                if buscar elemento izq then Rama(valor, eliminar elemento izq, der)
                else Rama(valor, izq, eliminar elemento der)

// Definimos una función recursiva para imprimir un árbol en forma de texto.
let imprimir (arbol : 'a arbol) : string =
    match arbol with
        | Hoja(valor) -> valor.ToString()
        | Rama(valor, izq, der) ->
            valor.ToString() + " (" + imprimir izq + ", " + imprimir der + ")"

// Creamos un árbol de ejemplo.
let arbol_ejemplo =
    Rama(1,
        Rama(2,
            Hoja(4),
            Hoja(5)),
        Rama(3,
            Hoja(6),
            Hoja(7)))

// Imprimimos el árbol.
printfn "%s" (imprimir arbol_ejemplo)

// Calculamos la altura del árbol.
let altura_arbol = altura arbol_ejemplo
printfn "Altura del árbol: %d" altura_arbol

// Buscamos un elemento en el árbol.
let elemento_a_buscar = 5
let encontrado = buscar elemento_a_buscar arbol_ejemplo
printfn "Elemento %d encontrado: %b" elemento_a_buscar encontrado

// Eliminamos un elemento del árbol.
let arbol_sin_elemento = eliminar elemento_a_buscar arbol_ejemplo

// Imprimimos el árbol sin el elemento.
printfn "%s" (imprimir arbol_sin_elemento)

```

Este código define un tipo de datos abstracto para representar un árbol binario, así como varias funciones para trabajar con árboles binarios, como calcular la altura de un árbol, buscar un elemento en un árbol y eliminar un elemento de un árbol. También incluye una función para imprimir un árbol en forma de texto.

El código se puede utilizar para crear y manipular árboles binarios. Por ejemplo, se puede crear un árbol binario de ejemplo, calcular su altura, buscar un elemento en el árbol y eliminar un elemento del árbol.

El código también se puede utilizar para implementar algoritmos que utilicen árboles binarios, como el algoritmo de búsqueda binaria o el algoritmo de ordenamiento por árbol binario.