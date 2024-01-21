```f#
// Módulo que define un tipo de dato para representar un árbol binario de búsqueda (ABB)
module BinarySearchTree =

    // Definición del tipo de dato ABB
    type ABB = Leaf | Node of int * ABB * ABB

    // Función para crear un ABB a partir de una lista de valores
    let crearABB (valores : int list) : ABB =
        match valores with
        | [] -> Leaf
        | h::t -> Node(h, crearABB t, crearABB [])

    // Función para insertar un valor en un ABB
    let insertar (valor : int) (abb : ABB) : ABB =
        match abb with
        | Leaf -> Node(valor, Leaf, Leaf)
        | Node(h, izq, der) when valor < h -> Node(h, insertar valor izq, der)
        | Node(h, izq, der) when valor > h -> Node(h, izq, insertar valor der)
        | _ -> abb

    // Función para eliminar un valor de un ABB
    let eliminar (valor : int) (abb : ABB) : ABB =
        match abb with
        | Leaf -> Leaf
        | Node(h, izq, der) when valor < h -> Node(h, eliminar valor izq, der)
        | Node(h, izq, der) when valor > h -> Node(h, izq, eliminar valor der)
        | Node(h, izq, der) ->
            let minDer = minimo der
            Node(minDer, eliminar minDer izq, der)

    // Función para obtener el valor mínimo de un ABB
    let minimo (abb : ABB) : int =
        match abb with
        | Leaf -> failwith "ABB vacío"
        | Node(h, izq, _) -> if izq = Leaf then h else minimo izq

    // Función para obtener el valor máximo de un ABB
    let maximo (abb : ABB) : int =
        match abb with
        | Leaf -> failwith "ABB vacío"
        | Node(h, _, der) -> if der = Leaf then h else maximo der

    // Función para buscar un valor en un ABB
    let buscar (valor : int) (abb : ABB) : bool =
        match abb with
        | Leaf -> false
        | Node(h, izq, der) when valor < h -> buscar valor izq
        | Node(h, izq, der) when valor > h -> buscar valor der
        | Node(h, _, _) when valor = h -> true


// Ejemplo de uso del módulo BinarySearchTree
let miABB = crearABB [10, 5, 15, 3, 7, 12, 20]

insertar 8 miABB

eliminar 10 miABB

buscar 15 miABB // true
buscar 25 miABB // false

minimo miABB // 3
maximo miABB // 20
```

Explicación del código:

* El módulo `BinarySearchTree` define un tipo de dato para representar un árbol binario de búsqueda (ABB). Un ABB es un árbol binario en el que las claves de los nodos están ordenadas de tal manera que la clave de cada nodo es mayor que las claves de todos sus nodos hijos de la izquierda y menor que las claves de todos sus nodos hijos de la derecha.
* El tipo de dato `ABB` se define como una unión de dos tipos: `Leaf` y `Node`. El tipo `Leaf` representa un árbol vacío, mientras que el tipo `Node` representa un árbol que tiene una raíz y dos subárboles, uno a la izquierda y otro a la derecha.
* La función `crearABB` toma una lista de valores y devuelve un ABB que contiene esos valores. La función funciona de forma recursiva, creando un ABB a partir de la cabeza y la cola de la lista. Si la lista está vacía, la función devuelve un árbol vacío. De lo contrario, la función crea un nodo con la cabeza de la lista como clave, y llama a sí misma de forma recursiva para crear los subárboles izquierdo y derecho.
* La función `insertar` toma un valor y un ABB y devuelve un nuevo ABB que contiene el valor insertado. La función funciona de forma recursiva, buscando el lugar correcto para insertar el valor en el árbol. Si el valor es menor que la clave del nodo actual, la función llama a sí misma de forma recursiva para insertar el valor en el subárbol izquierdo. Si el valor es mayor que la clave del nodo actual, la función llama a sí misma de forma recursiva para insertar el valor en el subárbol derecho. Si el valor es igual a la clave del nodo actual, la función simplemente devuelve el ABB actual.
* La función `eliminar` toma un valor y un ABB y devuelve un nuevo ABB que no contiene el valor eliminado. La función funciona de forma recursiva, buscando el nodo que contiene el valor a eliminar. Si el nodo no tiene hijos, la función simplemente lo elimina. Si el nodo tiene un hijo, la función reemplaza el nodo con su hijo. Si el nodo tiene dos hijos, la función encuentra el valor mínimo del subárbol derecho y lo sustituye por el valor del nodo a eliminar. A continuación, la función elimina el valor mínimo del subárbol derecho.
* La función `mínimo` toma un ABB y devuelve el valor mínimo del árbol. La función funciona de forma recursiva, buscando el nodo más a la izquierda del árbol. El valor del nodo más a la izquierda es el valor mínimo del árbol.
* La función `máximo` toma un ABB y devuelve el valor máximo del árbol. La función funciona de forma recursiva, buscando el nodo más a la derecha del árbol. El valor del nodo más a la derecha es el valor máximo del árbol.
* La función `buscar` toma un valor y un ABB y devuelve un valor booleano que indica si el valor está presente en el árbol. La función funciona de forma recursiva, buscando el nodo que contiene el valor. Si el nodo no existe, la función devuelve `false`. De lo contrario, la función devuelve `true`.

El código también incluye un ejemplo de uso del módulo `BinarySearchTree`. En este ejemplo, se crea un ABB a partir de una lista de valores, se insertan y eliminan algunos valores, y se buscan algunos valores en el árbol.