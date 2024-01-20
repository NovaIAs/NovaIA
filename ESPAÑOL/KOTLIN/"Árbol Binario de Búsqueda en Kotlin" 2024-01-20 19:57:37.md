```kotlin
// Clase para representar un árbol binario de búsqueda
class ArbolBinarioBusqueda<T : Comparable<T>> {

    // Raíz del árbol
    private var raiz: Nodo<T>? = null

    // Clase para representar un nodo del árbol
    private class Nodo<T : Comparable<T>>(var valor: T) {
        var izquierdo: Nodo<T>? = null
        var derecho: Nodo<T>? = null
    }

    // Método para insertar un nuevo valor en el árbol
    fun insertar(valor: T) {
        if (raiz == null) {
            raiz = Nodo(valor)
        } else {
            insertarRecursivo(valor, raiz!!)
        }
    }

    // Método recursivo para insertar un nuevo valor en el árbol
    private fun insertarRecursivo(valor: T, nodoActual: Nodo<T>) {
        if (valor < nodoActual.valor) {
            if (nodoActual.izquierdo == null) {
                nodoActual.izquierdo = Nodo(valor)
            } else {
                insertarRecursivo(valor, nodoActual.izquierdo!!)
            }
        } else {
            if (nodoActual.derecho == null) {
                nodoActual.derecho = Nodo(valor)
            } else {
                insertarRecursivo(valor, nodoActual.derecho!!)
            }
        }
    }

    // Método para buscar un valor en el árbol
    fun buscar(valor: T): Boolean {
        return buscarRecursivo(valor, raiz)
    }

    // Método recursivo para buscar un valor en el árbol
    private fun buscarRecursivo(valor: T, nodoActual: Nodo<T>?): Boolean {
        if (nodoActual == null) {
            return false
        }

        if (valor == nodoActual.valor) {
            return true
        }

        if (valor < nodoActual.valor) {
            return buscarRecursivo(valor, nodoActual.izquierdo)
        } else {
            return buscarRecursivo(valor, nodoActual.derecho)
        }
    }

    // Método para eliminar un valor del árbol
    fun eliminar(valor: T) {
        eliminarRecursivo(valor, raiz)
    }

    // Método recursivo para eliminar un valor del árbol
    private fun eliminarRecursivo(valor: T, nodoActual: Nodo<T>?) {
        if (nodoActual == null) {
            return
        }

        if (valor == nodoActual.valor) {
            eliminarNodo(nodoActual)
        } else if (valor < nodoActual.valor) {
            eliminarRecursivo(valor, nodoActual.izquierdo)
        } else {
            eliminarRecursivo(valor, nodoActual.derecho)
        }
    }

    // Método para eliminar un nodo del árbol
    private fun eliminarNodo(nodoActual: Nodo<T>) {
        // Si el nodo no tiene hijos, simplemente lo eliminamos
        if (nodoActual.izquierdo == null && nodoActual.derecho == null) {
            eliminarNodoSinHijos(nodoActual)
        }
        // Si el nodo solo tiene un hijo, lo hacemos hijo del padre del nodo actual
        else if (nodoActual.izquierdo == null) {
            eliminarNodoConUnHijo(nodoActual)
        } else if (nodoActual.derecho == null) {
            eliminarNodoConUnHijo(nodoActual)
        }
        // Si el nodo tiene dos hijos, lo reemplazamos por el nodo más pequeño de su subárbol derecho
        else {
            eliminarNodoConDosHijos(nodoActual)
        }
    }

    // Método para eliminar un nodo sin hijos
    private fun eliminarNodoSinHijos(nodoActual: Nodo<T>) {
        // Si el nodo es la raíz del árbol, simplemente lo eliminamos
        if (nodoActual == raiz) {
            raiz = null
        }
        // Si el nodo no es la raíz, lo eliminamos de su padre
        else {
            val padre = encontrarPadre(nodoActual)
            if (padre!!.izquierdo == nodoActual) {
                padre.izquierdo = null
            } else {
                padre.derecho = null
            }
        }
    }

    // Método para eliminar un nodo con un hijo
    private fun eliminarNodoConUnHijo(nodoActual: Nodo<T>) {
        // Si el nodo es la raíz del árbol, lo reemplazamos por su hijo
        if (nodoActual == raiz) {
            raiz = nodoActual.izquierdo ?: nodoActual.derecho
        }
        // Si el nodo no es la raíz, lo eliminamos de su padre y lo reemplazamos por su hijo
        else {
            val padre = encontrarPadre(nodoActual)
            if (padre!!.izquierdo == nodoActual) {
                padre.izquierdo = nodoActual.izquierdo ?: nodoActual.derecho
            } else {
                padre.derecho = nodoActual.izquierdo ?: nodoActual.derecho
            }
        }
    }

    // Método para eliminar un nodo con dos hijos
    private fun eliminarNodoConDosHijos(nodoActual: Nodo<T>) {
        // Buscamos el nodo más pequeño del subárbol derecho del nodo actual
        val nodoMinimo = encontrarNodoMinimo(nodoActual.derecho!!)

        // Reemplazamos el valor del nodo actual por el valor del nodo mínimo
        nodoActual.valor = nodoMinimo.valor

        // Eliminamos el nodo mínimo del subárbol derecho del nodo actual
        eliminarNodo(nodoMinimo)
    }

    // Método para encontrar el padre de un nodo
    private fun encontrarPadre(nodoActual: Nodo<T>): Nodo<T>? {
        // Si el nodo actual es la raíz del árbol, no tiene padre
        if (nodoActual == raiz) {
            return null
        }

        // Recorremos el árbol hasta encontrar el padre del nodo actual
        var padre: Nodo<T>? = raiz
        while (padre!!.izquierdo != nodoActual && padre.derecho != nodoActual) {
            if (nodoActual.valor < padre.valor) {
                padre = padre.izquierdo
            } else {
                padre = padre.derecho
            }
        }

        return padre
    }

    // Método para encontrar el nodo más pequeño de un subárbol
    private fun encontrarNodoMinimo(subarbol: Nodo<T>): Nodo<T> {
        // Recorremos el subárbol hasta llegar al nodo más pequeño
        var nodoActual = subarbol
        while (nodoActual.izquierdo != null) {
            nodoActual = nodoActual.izquierdo!!
        }

        return nodoActual
    }

    // Método para imprimir el árbol en preorden
    fun imprimirPreorden() {
        imprimirPreordenRecursivo(raiz)
    }

    // Método recursivo para imprimir el árbol en preorden
    private fun imprimirPreordenRecursivo(nodoActual: Nodo<T>?) {
        if (nodoActual != null) {
            print("${nodoActual.valor} ")
            imprimirPreordenRecursivo(nodoActual.izquierdo)
            imprimirPreordenRecursivo(nodoActual.derecho)
        }
    }

    // Método para imprimir el árbol en inorden
    fun imprimirInorden() {
        imprimirInordenRecursivo(raiz)
    }

    // Método recursivo para imprimir el árbol en inorden
    private fun imprimirInordenRecursivo(nodoActual: Nodo<T>?) {
        if (nodoActual != null) {
            imprimirInordenRecursivo(nodoActual.izquierdo)
            print("${nodoActual.valor} ")
            imprimirInordenRecursivo(nodoActual.derecho)
        }
    }

    // Método para imprimir el árbol en postorden
    fun imprimirPostorden() {
