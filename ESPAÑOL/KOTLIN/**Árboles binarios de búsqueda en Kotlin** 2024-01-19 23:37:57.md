```kotlin
// Creación de una clase para representar un árbol binario de búsqueda
class ArbolBinarioDeBusqueda<T : Comparable<T>> {

    // Atributos de la clase
    var raiz: Nodo<T>? = null

    // Nodo interno de un árbol binario de búsqueda
    inner class Nodo<T : Comparable<T>>(var valor: T) {
        var izquierda: Nodo<T>? = null
        var derecha: Nodo<T>? = null
    }

    // Insertar un valor en el árbol
    fun insertar(valor: T) {
        if (raiz == null) {
            raiz = Nodo(valor)
        } else {
            insertarRecursivo(raiz!!, valor)
        }
    }

    // Insertar recursivamente un valor en el árbol
    private fun insertarRecursivo(nodo: Nodo<T>, valor: T) {
        if (valor < nodo.valor) {
            if (nodo.izquierda == null) {
                nodo.izquierda = Nodo(valor)
            } else {
                insertarRecursivo(nodo.izquierda!!, valor)
            }
        } else {
            if (nodo.derecha == null) {
                nodo.derecha = Nodo(valor)
            } else {
                insertarRecursivo(nodo.derecha!!, valor)
            }
        }
    }

    // Buscar un valor en el árbol
    fun buscar(valor: T): Nodo<T>? {
        return buscarRecursivo(raiz, valor)
    }

    // Buscar recursivamente un valor en el árbol
    private fun buscarRecursivo(nodo: Nodo<T>?, valor: T): Nodo<T>? {
        if (nodo == null) {
            return null
        }

        if (nodo.valor == valor) {
            return nodo
        }

        if (valor < nodo.valor) {
            return buscarRecursivo(nodo.izquierda, valor)
        } else {
            return buscarRecursivo(nodo.derecha, valor)
        }
    }

    // Eliminar un valor del árbol
    fun eliminar(valor: T) {
        raiz = eliminarRecursivo(raiz, valor)
    }

    // Eliminar recursivamente un valor del árbol
    private fun eliminarRecursivo(nodo: Nodo<T>?, valor: T): Nodo<T>? {
        if (nodo == null) {
            return null
        }

        if (valor < nodo.valor) {
            nodo.izquierda = eliminarRecursivo(nodo.izquierda, valor)
        } else if (valor > nodo.valor) {
            nodo.derecha = eliminarRecursivo(nodo.derecha, valor)
        } else {
            if (nodo.izquierda == null) {
                return nodo.derecha
            } else if (nodo.derecha == null) {
                return nodo.izquierda
            }

            nodo.valor = encontrarSucesor(nodo.derecha!!).valor
            nodo.derecha = eliminarRecursivo(nodo.derecha, nodo.valor)
        }

        return nodo
    }

    // Encontrar el nodo con el valor más pequeño en un subárbol
    private fun encontrarPredecesor(nodo: Nodo<T>): Nodo<T> {
        var actual = nodo
        while (actual.izquierda != null) {
            actual = actual.izquierda!!
        }
        return actual
    }

    // Encontrar el nodo con el valor más grande en un subárbol
    private fun encontrarSucesor(nodo: Nodo<T>): Nodo<T> {
        var actual = nodo
        while (actual.derecha != null) {
            actual = actual.derecha!!
        }
        return actual
    }

    // Recorrer el árbol en orden (izquierda, raíz, derecha)
    fun recorrerEnOrden(visitador: (T) -> Unit) {
        recorrerEnOrdenRecursivo(raiz, visitador)
    }

    // Recorrer recursivamente el árbol en orden
    private fun recorrerEnOrdenRecursivo(nodo: Nodo<T>?, visitador: (T) -> Unit) {
        if (nodo != null) {
            recorrerEnOrdenRecursivo(nodo.izquierda, visitador)
            visitador(nodo.valor)
            recorrerEnOrdenRecursivo(nodo.derecha, visitador)
        }
    }

    // Recorrer el árbol en preorden (raíz, izquierda, derecha)
    fun recorrerEnPreOrden(visitador: (T) -> Unit) {
        recorrerEnPreOrdenRecursivo(raiz, visitador)
    }

    // Recorrer recursivamente el árbol en preorden
    private fun recorrerEnPreOrdenRecursivo(nodo: Nodo<T>?, visitador: (T) -> Unit) {
        if (nodo != null) {
            visitador(nodo.valor)
            recorrerEnPreOrdenRecursivo(nodo.izquierda, visitador)
            recorrerEnPreOrdenRecursivo(nodo.derecha, visitador)
        }
    }

    // Recorrer el árbol en postorden (izquierda, derecha, raíz)
    fun recorrerEnPostOrden(visitador: (T) -> Unit) {
        recorrerEnPostOrdenRecursivo(raiz, visitador)
    }

    // Recorrer recursivamente el árbol en postorden
    private fun recorrerEnPostOrdenRecursivo(nodo: Nodo<T>?, visitador: (T) -> Unit) {
        if (nodo != null) {
            recorrerEnPostOrdenRecursivo(nodo.izquierda, visitador)
            recorrerEnPostOrdenRecursivo(nodo.derecha, visitador)
            visitador(nodo.valor)
        }
    }
}

fun main() {
    // Crear un árbol binario de búsqueda
    val arbol = ArbolBinarioDeBusqueda<Int>()

    // Insertar valores en el árbol
    arbol.insertar(10)
    arbol.insertar(5)
    arbol.insertar(15)
    arbol.insertar(2)
    arbol.insertar(7)
    arbol.insertar(12)
    arbol.insertar(20)

    // Buscar un valor en el árbol
    val nodoBuscado = arbol.buscar(15)
    if (nodoBuscado != null) {
        println("Valor encontrado: ${nodoBuscado.valor}")
    } else {
        println("Valor no encontrado")
    }

    // Recorrer el árbol en orden
    println("Recorrido en orden:")
    arbol.recorrerEnOrden { valor -> print("$valor ") }
    println()

    // Recorrer el árbol en preorden
    println("Recorrido en preorden:")
    arbol.recorrerEnPreOrden { valor -> print("$valor ") }
    println()

    // Recorrer el árbol en postorden
    println("Recorrido en postorden:")
    arbol.recorrerEnPostOrden { valor -> print("$valor ") }
    println()

    // Eliminar un valor del árbol
    arbol.eliminar(15)

    // Buscar el valor eliminado en el árbol
    val nodoEliminado = arbol.buscar(15)
    if (nodoEliminado == null) {
        println("Valor eliminado: 15")
    } else {
        println("Valor no eliminado")
    }

    // Recorrer el árbol en orden después de eliminar el valor
    println("Recorrido en orden después de eliminar el valor:")
    arbol.recorrerEnOrden { valor -> print("$valor ") }
    println()
}
```

Explicación del código:

- La clase `ArbolBinarioDeBusqueda` representa un árbol binario de búsqueda, con métodos para insertar, buscar, eliminar y recorrer el árbol.


- La clase `Nodo` representa un nodo del árbol, con un valor y referencias a los nodos izquierdo y derecho.


- El método `insertar` inserta un valor en el árbol. Si el árbol está vacío, el valor se convierte en la raíz. Si el valor es menor que el valor de la raíz, se inserta en el subárbol izquierdo. Si el valor es mayor que el valor de la raíz, se inserta en el subárbol derecho.


- El método `buscar` busca un valor en el árbol. Si el valor se encuentra en la raíz, se devuelve el nodo raíz. Si el valor es menor que el valor de la raíz, se busca en el subárbol izquierdo. Si el valor es mayor que el valor de la raíz, se busca en el subárbol derecho.


- El método `eliminar` elimina un valor del árbol. Si el valor se encuentra en la raíz, se reemplaza por el sucesor más pequeño. Si el valor se encuentra en un nodo interno, se reemplaza por el predecesor más grande.


- Los métodos `recorrerEnOrden`, `recorrerEnPreOrden` y `recorrerEnPostOrden` recorren el árbol en orden, preorden y postorden respectivamente.


En el método `main` se crea un árbol binario de búsqueda, se insertan valores, se busca un valor, se elimina un valor y se recorre el árbol en orden, preorden y postorden.