```kotlin
// Clase para representar un árbol binario de búsqueda
class ArbolBinario(var raiz: Nodo? = null) {

    // Clase para representar un nodo del árbol
    class Nodo(var dato: Int, var izquierdo: Nodo? = null, var derecho: Nodo? = null) {
        // Función para insertar un nuevo elemento en el árbol
        fun insertar(nuevoDato: Int) {
            if (nuevoDato < dato) {
                if (izquierdo == null) {
                    izquierdo = Nodo(nuevoDato)
                } else {
                    izquierdo!!.insertar(nuevoDato)
                }
            } else {
                if (derecho == null) {
                    derecho = Nodo(nuevoDato)
                } else {
                    derecho!!.insertar(nuevoDato)
                }
            }
        }

        // Función para buscar un elemento en el árbol
        fun buscar(datoABuscar: Int): Nodo? {
            if (datoABuscar == dato) {
                return this
            } else if (datoABuscar < dato) {
                if (izquierdo == null) {
                    return null
                } else {
                    return izquierdo!!.buscar(datoABuscar)
                }
            } else {
                if (derecho == null) {
                    return null
                } else {
                    return derecho!!.buscar(datoABuscar)
                }
            }
        }

        // Función para eliminar un elemento del árbol
        fun eliminar(datoAEliminar: Int) {
            if (datoAEliminar < dato) {
                if (izquierdo != null) {
                    if (izquierdo!!.dato == datoAEliminar) {
                        izquierdo = null
                    } else {
                        izquierdo!!.eliminar(datoAEliminar)
                    }
                }
            } else if (datoAEliminar > dato) {
                if (derecho != null) {
                    if (derecho!!.dato == datoAEliminar) {
                        derecho = null
                    } else {
                        derecho!!.eliminar(datoAEliminar)
                    }
                }
            } else {
                if (izquierdo == null && derecho == null) {
                    raiz = null
                } else if (izquierdo == null) {
                    raiz = derecho
                } else if (derecho == null) {
                    raiz = izquierdo
                } else {
                    var nodoSucesor = izquierdo
                    while (nodoSucesor!!.derecho != null) {
                        nodoSucesor = nodoSucesor!!.derecho
                    }
                    dato = nodoSucesor!!.dato
                    nodoSucesor = null
                }
            }
        }

        // Función para recorrer el árbol en preorden
        fun recorrerPreorden(visitante: (Int) -> Unit) {
            visitante(dato)
            izquierdo?.recorrerPreorden(visitante)
            derecho?.recorrerPreorden(visitante)
        }

        // Función para recorrer el árbol en inorden
        fun recorrerInorden(visitante: (Int) -> Unit) {
            izquierdo?.recorrerInorden(visitante)
            visitante(dato)
            derecho?.recorrerInorden(visitante)
        }

        // Función para recorrer el árbol en postorden
        fun recorrerPostorden(visitante: (Int) -> Unit) {
            izquierdo?.recorrerPostorden(visitante)
            derecho?.recorrerPostorden(visitante)
            visitante(dato)
        }
    }

    // Función para insertar un nuevo elemento en el árbol
    fun insertar(nuevoDato: Int) {
        if (raiz == null) {
            raiz = Nodo(nuevoDato)
        } else {
            raiz!!.insertar(nuevoDato)
        }
    }

    // Función para buscar un elemento en el árbol
    fun buscar(datoABuscar: Int): Nodo? {
        return raiz?.buscar(datoABuscar)
    }

    // Función para eliminar un elemento del árbol
    fun eliminar(datoAEliminar: Int) {
        if (raiz != null) {
            raiz!!.eliminar(datoAEliminar)
        }
    }

    // Función para recorrer el árbol en preorden
    fun recorrerPreorden(visitante: (Int) -> Unit) {
        raiz?.recorrerPreorden(visitante)
    }

    // Función para recorrer el árbol en inorden
    fun recorrerInorden(visitante: (Int) -> Unit) {
        raiz?.recorrerInorden(visitante)
    }

    // Función para recorrer el árbol en postorden
    fun recorrerPostorden(visitante: (Int) -> Unit) {
        raiz?.recorrerPostorden(visitante)
    }
}

// Función para crear un árbol binario de búsqueda a partir de una lista de elementos
fun crearArbolBinario(lista: List<Int>): ArbolBinario {
    val arbol = ArbolBinario()
    lista.forEach { arbol.insertar(it) }
    return arbol
}

// Función para imprimir un árbol binario de búsqueda en forma de cadena
fun imprimirArbolBinario(arbol: ArbolBinario): String {
    val builder = StringBuilder()
    arbol.recorrerInorden { builder.append("$it ") }
    return builder.toString().trim()
}

// Función principal
fun main(args: Array<String>) {
    // Crear un árbol binario de búsqueda a partir de una lista de elementos
    val arbol = crearArbolBinario(listOf(10, 5, 15, 3, 7, 12, 20))

    // Imprimir el árbol binario de búsqueda en forma de cadena
    println(imprimirArbolBinario(arbol))

    // Buscar un elemento en el árbol binario de búsqueda
    val elementoABuscar = 12
    val nodoEncontrado = arbol.buscar(elementoABuscar)
    if (nodoEncontrado != null) {
        println("El elemento $elementoABuscar se encontró en el árbol binario de búsqueda")
    } else {
        println("El elemento $elementoABuscar no se encontró en el árbol binario de búsqueda")
    }

    // Eliminar un elemento del árbol binario de búsqueda
    val elementoAEliminar = 15
    arbol.eliminar(elementoAEliminar)

    // Imprimir el árbol binario de búsqueda en forma de cadena
    println(imprimirArbolBinario(arbol))
}
```

Explicación del código:

* La clase `ArbolBinario` representa un árbol binario de búsqueda. Tiene un atributo `raiz` que apunta al nodo raíz del árbol.

* La clase `Nodo` representa un nodo del árbol binario de búsqueda. Tiene un atributo `dato` que almacena el valor del nodo, y dos atributos `izquierdo` y `derecho` que apuntan a los nodos izquierdo y derecho del nodo, respectivamente.

* La función `insertar` inserta un nuevo elemento en el árbol binario de búsqueda. Si el árbol está vacío, el nuevo elemento se convierte en el nodo raíz. De lo contrario, el nuevo elemento se inserta en el nodo izquierdo o derecho del nodo actual, dependiendo del valor del nuevo elemento.

* La función `buscar` busca un elemento en el árbol binario de búsqueda. Si el árbol está vacío, la función devuelve `null`. De lo contrario, la función busca el elemento en el nodo actual y en sus nodos izquierdo y derecho, hasta encontrarlo o llegar a un nodo vacío. Si el elemento se encuentra, la función devuelve el nodo que contiene el elemento. De lo contrario, la función devuelve `null`.

* La función `eliminar` elimina un elemento del árbol binario de búsqueda. Si el árbol está vacío, la función hace nada. De lo contrario, la función elimina el elemento en el nodo actual y sus nodos izquierdo y derecho, hasta llegar a un nodo vacío. Si el nodo actual no tiene nodos izquierdo y derecho, la función simplemente elimina el nodo actual. Si el nodo actual tiene un nodo izquierdo o derecho, la función sustituye el nodo actual por su nodo izquierdo o derecho, respectivamente.

* Las funciones `recorrerPreorden`, `recorrerInorden` y `recorrerPostorden` recorren el árbol binario de búsqueda en preorden, inorden y postorden, respectivamente. La función `recorrerPreorden` visita primero el nodo actual, luego el nodo izquierdo y finalmente el nodo derecho. La función `recorrerInorden` visita primero el nodo izquierdo, luego el nodo actual y finalmente el nodo derecho. La función `recorrerPostorden` visita primero el nodo izquierdo, luego el nodo derecho y finalmente el nodo actual.

* La función `crearArbolBinario` crea un árbol binario de búsqueda a partir de una lista de elementos. La función recorre la lista y llama a la función `insertar` para insertar cada elemento en el árbol binario de búsqueda.

* La función `imprimirArbolBinario` imprime el árbol binario de búsqueda en forma de cadena. La función recorre el árbol binario de búsqueda en inorden y concatena los valores de los nodos en una cadena.

* La función `main` es el punto de entrada del programa. La función crea un árbol binario de búsqueda a partir de una lista de elementos, imprime el árbol binario de búsqueda en forma de cadena, busca un elemento en el árbol binario de búsqueda y elimina un elemento del árbol binario de búsqueda.