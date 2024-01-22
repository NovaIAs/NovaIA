```groovy
// Clase para representar un árbol binario de búsqueda
class ArbolBinario {

    // Raíz del árbol
    Nodo raiz

    // Método para insertar un nuevo nodo en el árbol
    void insertar(Object dato) {
        // Si el árbol está vacío, el nuevo nodo se convierte en la raíz
        if (raiz == null) {
            raiz = new Nodo(dato)
        } else {
            // Si el árbol no está vacío, se llama a un método recursivo
            // para insertar el nuevo nodo en el lugar correcto
            insertarRecursivo(raiz, dato)
        }
    }

    // Método recursivo para insertar un nuevo nodo en el árbol
    private void insertarRecursivo(Nodo nodoActual, Object dato) {
        // Si el dato es menor que el dato del nodo actual, se inserta
        // en la rama izquierda
        if (dato < nodoActual.dato) {
            // Si la rama izquierda está vacía, se crea un nuevo nodo
            // y se inserta el dato
            if (nodoActual.nodoIzquierdo == null) {
                nodoActual.nodoIzquierdo = new Nodo(dato)
            } else {
                // Si la rama izquierda no está vacía, se llama al método
                // recursivo para insertar el dato en la rama izquierda
                insertarRecursivo(nodoActual.nodoIzquierdo, dato)
            }
        } else {
            // Si el dato es mayor que el dato del nodo actual, se inserta
            // en la rama derecha
            if (nodoActual.nodoDerecho == null) {
                nodoActual.nodoDerecho = new Nodo(dato)
            } else {
                // Si la rama derecha no está vacía, se llama al método
                // recursivo para insertar el dato en la rama derecha
                insertarRecursivo(nodoActual.nodoDerecho, dato)
            }
        }
    }

    // Método para buscar un nodo en el árbol
    Nodo buscar(Object dato) {
        // Si el árbol está vacío, se devuelve null
        if (raiz == null) {
            return null
        } else {
            // Si el árbol no está vacío, se llama a un método recursivo
            // para buscar el nodo en el lugar correcto
            return buscarRecursivo(raiz, dato)
        }
    }

    // Método recursivo para buscar un nodo en el árbol
    private Nodo buscarRecursivo(Nodo nodoActual, Object dato) {
        // Si el dato es igual al dato del nodo actual, se devuelve el nodo actual
        if (dato == nodoActual.dato) {
            return nodoActual
        } else if (dato < nodoActual.dato) {
            // Si el dato es menor que el dato del nodo actual, se llama al método
            // recursivo para buscar el dato en la rama izquierda
            if (nodoActual.nodoIzquierdo == null) {
                return null
            } else {
                return buscarRecursivo(nodoActual.nodoIzquierdo, dato)
            }
        } else {
            // Si el dato es mayor que el dato del nodo actual, se llama al método
            // recursivo para buscar el dato en la rama derecha
            if (nodoActual.nodoDerecho == null) {
                return null
            } else {
                return buscarRecursivo(nodoActual.nodoDerecho, dato)
            }
        }
    }

    // Método para eliminar un nodo del árbol
    void eliminar(Object dato) {
        // Si el árbol está vacío, no se hace nada
        if (raiz == null) {
            return
        } else {
            // Si el árbol no está vacío, se llama a un método recursivo
            // para eliminar el nodo en el lugar correcto
            eliminarRecursivo(raiz, dato)
        }
    }

    // Método recursivo para eliminar un nodo del árbol
    private void eliminarRecursivo(Nodo nodoActual, Object dato) {
        // Si el dato es igual al dato del nodo actual, se elimina el nodo actual
        if (dato == nodoActual.dato) {
            // Si el nodo actual no tiene hijos, se elimina directamente
            if (nodoActual.nodoIzquierdo == null && nodoActual.nodoDerecho == null) {
                nodoActual = null
            } else if (nodoActual.nodoIzquierdo == null) {
                nodoActual = nodoActual.nodoDerecho
            } else if (nodoActual.nodoDerecho == null) {
                nodoActual = nodoActual.nodoIzquierdo
            } else {
                // Si el nodo actual tiene dos hijos, se busca el nodo más pequeño
                // en la rama derecha y se reemplaza el nodo actual con ese nodo
                Nodo nodoMaspequeno = nodoActual.nodoDerecho
                while (nodoMaspequeno.nodoIzquierdo != null) {
                    nodoMaspequeno = nodoMaspequeno.nodoIzquierdo
                }
                nodoActual.dato = nodoMaspequeno.dato
                eliminarRecursivo(nodoMaspequeno, nodoMaspequeno.dato)
            }
        } else if (dato < nodoActual.dato) {
            // Si el dato es menor que el dato del nodo actual, se llama al método
            // recursivo para eliminar el dato en la rama izquierda
            if (nodoActual.nodoIzquierdo == null) {
                return
            } else {
                eliminarRecursivo(nodoActual.nodoIzquierdo, dato)
            }
        } else {
            // Si el dato es mayor que el dato del nodo actual, se llama al método
            // recursivo para eliminar el dato en la rama derecha
            if (nodoActual.nodoDerecho == null) {
                return
            } else {