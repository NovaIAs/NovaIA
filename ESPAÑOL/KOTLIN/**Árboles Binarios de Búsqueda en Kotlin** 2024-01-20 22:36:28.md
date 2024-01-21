```kotlin
// Creemos una clase para representar un árbol binario de búsqueda.
class ArbolBinarioBusqueda {
    // El nodo raíz del árbol.
    var raiz: Nodo? = null

    // Función para insertar un nuevo elemento en el árbol.
    fun insertar(elemento: Int) {
        // Si el árbol está vacío, el nuevo elemento se convierte en la raíz.
        if (raiz == null) {
            raiz = Nodo(elemento)
            return
        }

        // De lo contrario, buscamos el lugar adecuado para insertar el nuevo elemento.
        var nodoActual = raiz
        while (true) {
            // Si el nuevo elemento es menor que el nodo actual, vamos al nodo izquierdo.
            if (elemento < nodoActual!!.elemento) {
                // Si el nodo izquierdo es nulo, hemos encontrado el lugar adecuado para insertar el nuevo elemento.
                if (nodoActual.izquierdo == null) {
                    nodoActual.izquierdo = Nodo(elemento)
                    return
                }
                // De lo contrario, seguimos buscando el lugar adecuado.
                nodoActual = nodoActual.izquierdo
            }
            // Si el nuevo elemento es mayor que el nodo actual, vamos al nodo derecho.
            else {
                // Si el nodo derecho es nulo, hemos encontrado el lugar adecuado para insertar el nuevo elemento.
                if (nodoActual.derecho == null) {
                    nodoActual.derecho = Nodo(elemento)
                    return
                }
                // De lo contrario, seguimos buscando el lugar adecuado.
                nodoActual = nodoActual.derecho
            }
        }
    }

    // Función para buscar un elemento en el árbol.
    fun buscar(elemento: Int): Nodo? {
        // Si el árbol está vacío, el elemento no está en el árbol.
        if (raiz == null) {
            return null
        }

        // De lo contrario, buscamos el elemento en el árbol.
        var nodoActual = raiz
        while (nodoActual != null) {
            // Si el elemento es igual al nodo actual, hemos encontrado el elemento.
            if (elemento == nodoActual!!.elemento) {
                return nodoActual
            }
            // Si el elemento es menor que el nodo actual, vamos al nodo izquierdo.
            else if (elemento < nodoActual.elemento) {
                nodoActual = nodoActual.izquierdo
            }
            // Si el elemento es mayor que el nodo actual, vamos al nodo derecho.
            else {
                nodoActual = nodoActual.derecho
            }
        }

        // Si hemos llegado aquí, el elemento no está en el árbol.
        return null
    }

    // Función para eliminar un elemento del árbol.
    fun eliminar(elemento: Int) {
        // Si el árbol está vacío, el elemento no está en el árbol.
        if (raiz == null) {
            return
        }

        // De lo contrario, buscamos el elemento en el árbol.
        var nodoActual = raiz
        var padreNodoActual: Nodo? = null
        while (nodoActual != null) {
            // Si el elemento es igual al nodo actual, hemos encontrado el elemento.
            if (elemento == nodoActual!!.elemento) {
                break
            }
            // Si el elemento es menor que el nodo actual, vamos al nodo izquierdo.
            else if (elemento < nodoActual.elemento) {
                padreNodoActual = nodoActual
                nodoActual = nodoActual.izquierdo
            }
            // Si el elemento es mayor que el nodo actual, vamos al nodo derecho.
            else {
                padreNodoActual = nodoActual
                nodoActual = nodoActual.derecho
            }
        }

        // Si el nodo actual es nulo, el elemento no está en el árbol.
        if (nodoActual == null) {
            return
        }

        // Si el nodo actual no tiene hijos, simplemente lo eliminamos.
        if (nodoActual.izquierdo == null && nodoActual.derecho == null) {
            // Si el nodo actual es la raíz del árbol, lo eliminamos y el árbol queda vacío.
            if (padreNodoActual == null) {
                raiz = null
                return
            }
            // De lo contrario, eliminamos el nodo actual y ajustamos el árbol en consecuencia.
            else {
                if (nodoActual == padreNodoActual!!.izquierdo) {
                    padreNodoActual.izquierdo = null
                } else {
                    padreNodoActual.derecho = null
                }
                return
            }
        }

        // Si el nodo actual tiene un hijo izquierdo, lo hacemos el hijo izquierdo del padre del nodo actual.
        if (nodoActual.izquierdo != null && nodoActual.derecho == null) {
            // Si el nodo actual es la raíz del árbol, lo eliminamos y el hijo izquierdo del nodo actual se convierte en la raíz del árbol.
            if (padreNodoActual == null) {
                raiz = nodoActual.izquierdo
                return
            }
            // De lo contrario, eliminamos el nodo actual y ajustamos el árbol en consecuencia.
            else {
                if (nodoActual == padreNodoActual!!.izquierdo) {
                    padreNodoActual.izquierdo = nodoActual.izquierdo
                } else {
                    padreNodoActual.derecho = nodoActual.izquierdo
                }
                return
            }
        }

        // Si el nodo actual tiene un hijo derecho, lo hacemos el hijo derecho del padre del nodo actual.
        if (nodoActual.izquierdo == null && nodoActual.derecho != null) {
            // Si el nodo actual es la raíz del árbol, lo eliminamos y el hijo derecho del nodo actual se convierte en la raíz del árbol.
            if (padreNodoActual == null) {
                raiz = nodoActual.derecho
                return
            }
            // De