```java
// Esta clase implementa un árbol binario de búsqueda en Java.
public class ArbolBinarioBusqueda<T extends Comparable<T>> {

    // Nodo raíz del árbol.
    private Nodo<T> raiz;

    // Clase interna que representa un nodo del árbol.
    private class Nodo<T> {

        // Dato almacenado en el nodo.
        private T dato;

        // Nodo izquierdo del nodo actual.
        private Nodo<T> izquierdo;

        // Nodo derecho del nodo actual.
        private Nodo<T> derecho;

        // Constructor del nodo.
        public Nodo(T dato) {
            this.dato = dato;
            this.izquierdo = null;
            this.derecho = null;
        }
    }

    // Método que inserta un dato en el árbol.
    public void insertar(T dato) {
        // Si el árbol está vacío, crea un nuevo nodo con el dato y lo asigna como raíz.
        if (raiz == null) {
            raiz = new Nodo<>(dato);
            return;
        }

        // Si el árbol no está vacío, llama al método insertar recursivo.
        insertarRecursivo(raiz, dato);
    }

    // Método recursivo que inserta un dato en el árbol.
    private void insertarRecursivo(Nodo<T> nodoActual, T dato) {
        // Si el dato es menor que el dato del nodo actual, se inserta en el nodo izquierdo.
        if (dato.compareTo(nodoActual.dato) < 0) {
            // Si el nodo izquierdo es nulo, se crea un nuevo nodo con el dato y se asigna como nodo izquierdo.
            if (nodoActual.izquierdo == null) {
                nodoActual.izquierdo = new Nodo<>(dato);
                return;
            }

            // Si el nodo izquierdo no es nulo, se llama al método insertar recursivo con el nodo izquierdo.
            else {
                insertarRecursivo(nodoActual.izquierdo, dato);
            }
        }

        // Si el dato es mayor o igual que el dato del nodo actual, se inserta en el nodo derecho.
        else {
            // Si el nodo derecho es nulo, se crea un nuevo nodo con el dato y se asigna como nodo derecho.
            if (nodoActual.derecho == null) {
                nodoActual.derecho = new Nodo<>(dato);
                return;
            }

            // Si el nodo derecho no es nulo, se llama al método insertar recursivo con el nodo derecho.
            else {
                insertarRecursivo(nodoActual.derecho, dato);
            }
        }
    }

    // Método que busca un dato en el árbol.
    public boolean buscar(T dato) {
        // Si el árbol está vacío, devuelve falso.
        if (raiz == null) {
            return false;
        }

        // Si el árbol no está vacío, llama al método buscar recursivo.
        return buscarRecursivo(raiz, dato);
    }

    // Método recursivo que busca un dato en el árbol.
    private boolean buscarRecursivo(Nodo<T> nodoActual, T dato) {
        // Si el dato es igual al dato del nodo actual, devuelve verdadero.
        if (dato.compareTo(nodoActual.dato) == 0) {
            return true;
        }

        // Si el dato es menor que el dato del nodo actual, se busca en el nodo izquierdo.
        else if (dato.compareTo(nodoActual.dato) < 0) {
            // Si el nodo izquierdo es nulo, devuelve falso.
            if (nodoActual.izquierdo == null) {
                return false;
            }

            // Si el nodo izquierdo no es nulo, se llama al método buscar recursivo con el nodo izquierdo.
            else {
                return buscarRecursivo(nodoActual.izquierdo, dato);
            }
        }

        // Si el dato es mayor que el dato del nodo actual, se busca en el nodo derecho.
        else {
            // Si el nodo derecho