```java
import java.util.*;
import java.lang.*;

public class CodigoComplejo {

    // Clase interna para representar un nodo de un árbol binario de búsqueda
    private static class Nodo {
        int valor;
        Nodo izquierda;
        Nodo derecha;

        public Nodo(int valor) {
            this.valor = valor;
            this.izquierda = null;
            this.derecha = null;
        }
    }

    // Método para insertar un nuevo nodo en un árbol binario de búsqueda
    public static Nodo insertarNodo(Nodo raiz, int valor) {
        if (raiz == null) {
            return new Nodo(valor);
        } else {
            if (valor < raiz.valor) {
                raiz.izquierda = insertarNodo(raiz.izquierda, valor);
            } else {
                raiz.derecha = insertarNodo(raiz.derecha, valor);
            }
            return raiz;
        }
    }

    // Método para buscar un nodo en un árbol binario de búsqueda
    public static Nodo buscarNodo(Nodo raiz, int valor) {
        if (raiz == null) {
            return null;
        } else {
            if (valor == raiz.valor) {
                return raiz;
            } else if (valor < raiz.valor) {
                return buscarNodo(raiz.izquierda, valor);
            } else {
                return buscarNodo(raiz.derecha, valor);
            }
        }
    }

    // Método para eliminar un nodo de un árbol binario de búsqueda
    public static Nodo eliminarNodo(Nodo raiz, int valor) {
        if (raiz == null) {
            return null;
        } else {
            if (valor == raiz.valor) {
                // Caso 1: Nodo hoja
                if (raiz.izquierda == null && raiz.derecha == null) {
                    return null;
                }
                // Caso 2: Nodo con un solo hijo
                else if (raiz.izquierda == null) {
                    return raiz.derecha;
                } else if (raiz.derecha == null) {
                    return raiz.izquierda;
                }
                // Caso 3: Nodo con dos hijos
                else {
                    // Buscar el nodo más pequeño en el subárbol derecho
                    Nodo nodoMinimo = raiz.derecha;
                    while (nodoMinimo.izquierda != null) {
                        nodoMinimo = nodoMinimo.izquierda;
                    }
                    // Reemplazar el valor del nodo a eliminar con el valor del nodo mínimo
                    raiz.valor = nodoMinimo.valor;
                    // Eliminar el nodo mínimo del subárbol derecho
                    raiz.derecha = eliminarNodo(raiz.derecha, nodoMinimo.valor);
                    return raiz;
                }
            } else if (valor < raiz.valor) {
                raiz.izquierda = eliminarNodo(raiz.izquierda, valor);
            } else {
                raiz.derecha = eliminarNodo(raiz.derecha, valor);
            }
            return raiz;
        }
    }

    // Método para imprimir un árbol binario de búsqueda en preorden
    public static void imprimirPreorden(Nodo raiz) {
        if (raiz != null) {
            System.out.print(raiz.valor + " ");
            imprimirPreorden(raiz.izquierda);
            imprimirPreorden(raiz.derecha);
        }
    }

    // Método para imprimir un árbol binario de búsqueda en inorden
    public static void imprimirInorden(Nodo raiz) {
        if (raiz != null) {
            imprimirInorden(raiz.izquierda);
            System.out.print(raiz.valor + " ");
            imprimirInorden(raiz.derecha);
        }
    }

    // Método para imprimir un árbol binario de búsqueda en postorden
    public static void imprimirPostorden(Nodo raiz) {
        if (raiz != null) {
            imprimirPostorden(raiz.izquierda);
            imprimirPostorden(raiz.derecha);
            System.out.print(raiz.valor + " ");
        }
    }

    // Método principal
    public static void main(String[] args) {
        // Crear un árbol binario de búsqueda
        Nodo raiz = null;
        raiz = insertarNodo(raiz, 10);
        raiz = insertarNodo(raiz, 5);
        raiz = insertarNodo(raiz, 15);
        raiz = insertarNodo(raiz, 2);
        raiz = insertarNodo(raiz, 7);
        raiz = insertarNodo(raiz, 12);
        raiz = insertarNodo(raiz, 20);

        // Buscar un nodo en el árbol binario de búsqueda
        Nodo nodo = buscarNodo(raiz, 12);
        if (nodo != null) {
            System.out.println("El nodo con valor 12 se encuentra en el árbol binario de búsqueda.");
        } else {
            System.out.println("El nodo con valor 12 no se encuentra en el árbol binario