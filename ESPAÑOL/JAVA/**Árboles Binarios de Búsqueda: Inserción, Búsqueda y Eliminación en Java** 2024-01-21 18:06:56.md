```java
import java.util.*;
import java.lang.*;

public class ComplejoCodigo {

    // Clase interna para representar un nodo de un árbol binario de búsqueda
    public class Nodo {
        int dato;
        Nodo izquierdo;
        Nodo derecho;

        public Nodo(int dato) {
            this.dato = dato;
            this.izquierdo = null;
            this.derecho = null;
        }
    }

    // Atributos de la clase ComplejoCodigo
    private Nodo raiz;
    private int tamaño;

    // Constructor de la clase ComplejoCodigo
    public ComplejoCodigo() {
        this.raiz = null;
        this.tamaño = 0;
    }

    // Método para insertar un nodo en el árbol binario de búsqueda
    public void insertar(int dato) {
        Nodo nuevoNodo = new Nodo(dato);
        Nodo actual = this.raiz;
        Nodo padre = null;

        while (actual != null) {
            padre = actual;
            if (dato < actual.dato) {
                actual = actual.izquierdo;
            } else if (dato > actual.dato) {
                actual = actual.derecho;
            } else {
                // El dato ya existe en el árbol binario de búsqueda
                return;
            }
        }

        if (padre == null) {
            this.raiz = nuevoNodo;
        } else if (dato < padre.dato) {
            padre.izquierdo = nuevoNodo;
        } else {
            padre.derecho = nuevoNodo;
        }

        this.tamaño++;
    }

    // Método para buscar un nodo en el árbol binario de búsqueda
    public boolean buscar(int dato) {
        Nodo actual = this.raiz;

        while (actual != null) {
            if (dato == actual.dato) {
                return true;
            } else if (dato < actual.dato) {
                actual = actual.izquierdo;
            } else {
                actual = actual.derecho;
            }
        }

        return false;
    }

    // Método para eliminar un nodo del árbol binario de búsqueda
    public void eliminar(int dato) {
        Nodo actual = this.raiz;
        Nodo padre = null;

        while (actual != null) {
            if (dato == actual.dato) {
                break;
            } else if (dato < actual.dato) {
                padre = actual;
                actual = actual.izquierdo;
            } else {
                padre = actual;
                actual = actual.derecho;
            }
        }

        if (actual == null) {
            // El dato no existe en el árbol binario de búsqueda
            return;
        }

        if (actual.izquierdo == null && actual.derecho == null) {
            // El nodo a eliminar es una hoja
            if (padre == null) {
                this.raiz = null;
            } else if (actual == padre.izquierdo) {
                padre.izquierdo = null;
            } else {
                padre.derecho = null;
            }
        } else if (actual.izquierdo != null && actual.derecho == null) {
            // El nodo a eliminar tiene un hijo izquierdo
            if (padre == null) {
                this.raiz = actual.izquierdo;
            } else if (actual == padre.izquierdo) {
                padre.izquierdo = actual.izquierdo;
            } else {
                padre.derecho = actual.izquierdo;
            }
        } else if (actual.izquierdo == null && actual.derecho != null) {
            // El nodo a eliminar tiene un hijo derecho
            if (padre == null) {
                this.raiz = actual.derecho;
            } else if (actual == padre.izquierdo) {
                padre.izquierdo = actual.derecho;
            } else {
                padre.derecho = actual.derecho;
            }
        } else {
            // El nodo a eliminar tiene dos hijos
            Nodo sucesor = actual.derecho;
            Nodo padreSucesor = actual;

            while (sucesor.izquierdo != null) {
                padreSucesor = sucesor;
                sucesor = sucesor.izquierdo;
            }

            actual.dato = sucesor.dato;

            if (padreSucesor == actual) {
                actual.derecho = sucesor.derecho;
            } else {
                padreSucesor.izquierdo = sucesor.derecho;
            }
        }

        this.tamaño--;
    }

    // Método para imprimir el árbol binario de búsqueda en preorden
    public void imprimirPreorden() {
        imprimirPreorden(this.raiz);
    }

    private void imprimirPreorden(Nodo nodo) {
        if (nodo != null) {
            System.out.print(nodo.dato + " ");
            imprimirPreorden(nodo.izquierdo);
            imprimirPreorden(nodo.derecho);