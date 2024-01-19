```javascript
// Función para crear un nuevo nodo en una lista enlazada
function nodo(valor) {
    return {
        valor: valor,
        siguiente: null
    };
}

// Función para insertar un nodo al final de una lista enlazada
function insertarFinal(cabeza, nuevoNodo) {
    if (cabeza === null) {
        return nuevoNodo;
    }
    let nodoActual = cabeza;
    while (nodoActual.siguiente !== null) {
        nodoActual = nodoActual.siguiente;
    }
    nodoActual.siguiente = nuevoNodo;
    return cabeza;
}

// Función para insertar un nodo al inicio de una lista enlazada
function insertarInicio(cabeza, nuevoNodo) {
    nuevoNodo.siguiente = cabeza;
    return nuevoNodo;
}

// Función para eliminar un nodo de una lista enlazada
function eliminarNodo(cabeza, valor) {
    if (cabeza === null) {
        return null;
    }
    if (cabeza.valor === valor) {
        return cabeza.siguiente;
    }
    let nodoActual = cabeza;
    while (nodoActual.siguiente !== null) {
        if (nodoActual.siguiente.valor === valor) {
            nodoActual.siguiente = nodoActual.siguiente.siguiente;
            return cabeza;
        }
        nodoActual = nodoActual.siguiente;
    }
    return cabeza;
}

// Función para buscar un nodo en una lista enlazada
function buscarNodo(cabeza, valor) {
    if (cabeza === null) {
        return null;
    }
    if (cabeza.valor === valor) {
        return cabeza;
    }
    let nodoActual = cabeza;
    while (nodoActual.siguiente !== null) {
        if (nodoActual.siguiente.valor === valor) {
            return nodoActual.siguiente;
        }
        nodoActual = nodoActual.siguiente;
    }
    return null;
}

// Función para imprimir una lista enlazada
function imprimirLista(cabeza) {
    if (cabeza === null) {
        console.log("Lista vacía");
    }
    let nodoActual = cabeza;
    while (nodoActual !== null) {
        console.log(nodoActual.valor);
        nodoActual = nodoActual.siguiente;
    }
}

// Función principal para probar las funciones de la lista enlazada
function main() {
    // Crear una lista enlazada
    let cabeza = insertarFinal(null, nodo(1));
    cabeza = insertarFinal(cabeza, nodo(2));
    cabeza = insertarFinal(cabeza, nodo(3));
    cabeza = insertarFinal(cabeza, nodo(4));
    cabeza = insertarFinal(cabeza, nodo(5));

    // Imprimir la lista enlazada
    console.log("Lista original:");
    imprimirLista(cabeza);

    // Insertar un nodo al final de la lista enlazada
    cabeza = insertarFinal(cabeza, nodo(6));

    // Imprimir la lista enlazada
    console.log("Lista después de insertar un nodo al final:");
    imprimirLista(cabeza);

    // Insertar un nodo al inicio de la lista enlazada
    cabeza = insertarInicio(cabeza, nodo(0));

    // Imprimir la lista enlazada
    console.log("Lista después de insertar un nodo al inicio:");
    imprimirLista(cabeza);

    // Eliminar un nodo de la lista enlazada
    cabeza = eliminarNodo(cabeza, 3);

    // Imprimir la lista enlazada
    console.log("Lista después de eliminar un nodo:");
    imprimirLista(cabeza);

    // Buscar un nodo en la lista enlazada
    let nodoEncontrado = buscarNodo(cabeza, 5);

    // Imprimir el nodo encontrado
    console.log("Nodo encontrado:");
    console.log(nodoEncontrado);
}

// Llamar a la función principal
main();
```

Explicación del código:

1. La función `nodo()` crea un nuevo nodo en la lista enlazada. Tiene un parámetro `valor` que se utiliza para almacenar el valor del nodo.
2. La función `insertarFinal()` inserta un nuevo nodo al final de la lista enlazada. Si la lista está vacía, el nuevo nodo se convierte en la cabeza de la lista. Si la lista no está vacía, la función recorre la lista hasta encontrar el último nodo y luego inserta el nuevo nodo después del último nodo.
3. La función `insertarInicio()` inserta un nuevo nodo al inicio de la lista enlazada. El nuevo nodo se convierte en la cabeza de la lista y el nodo anterior se convierte en el siguiente nodo del nuevo nodo.
4. La función `eliminarNodo()` elimina un nodo de la lista enlazada. La función recorre la lista hasta encontrar el nodo que se va a eliminar. Si el nodo se encuentra, se elimina y se devuelve la cabeza de la lista. Si el nodo no se encuentra, se devuelve la cabeza de la lista sin cambios.
5. La función `buscarNodo()` busca un nodo en la lista enlazada. La función recorre la lista hasta encontrar el nodo que se busca. Si el nodo se encuentra, se devuelve el nodo. Si el nodo no se encuentra, se devuelve `null`.
6. La función `imprimirLista()` imprime la lista enlazada. La función recorre la lista y imprime el valor de cada nodo.
7. La función `main()` es la función principal que prueba las funciones de la lista enlazada. La función crea una lista enlazada, inserta un nodo al final, inserta un nodo al inicio, elimina un nodo, busca un nodo e imprime la lista.