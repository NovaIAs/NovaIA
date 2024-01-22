```java
import java.util.*;
import java.lang.*;
import java.io.*;

public class CodigoComplejo {

    // Clase interna para representar un nodo de un árbol binario
    private static class Nodo {
        int dato;
        Nodo izquierdo;
        Nodo derecho;

        public Nodo(int dato) {
            this.dato = dato;
            this.izquierdo = null;
            this.derecho = null;
        }
    }

    // Método recursivo para agregar un nuevo nodo a un árbol binario
    public static void agregarNodo(Nodo raiz, int nuevoDato) {
        if (raiz == null) {
            raiz = new Nodo(nuevoDato);
        } else if (nuevoDato < raiz.dato) {
            agregarNodo(raiz.izquierdo, nuevoDato);
        } else {
            agregarNodo(raiz.derecho, nuevoDato);
        }
    }

    // Método recursivo para buscar un nodo en un árbol binario
    public static boolean buscarNodo(Nodo raiz, int dato) {
        if (raiz == null) {
            return false;
        } else if (raiz.dato == dato) {
            return true;
        } else if (dato < raiz.dato) {
            return buscarNodo(raiz.izquierdo, dato);
        } else {
            return buscarNodo(raiz.derecho, dato);
        }
    }

    // Método recursivo para eliminar un nodo de un árbol binario
    public static Nodo eliminarNodo(Nodo raiz, int dato) {
        if (raiz == null) {
            return raiz;
        } else if (dato < raiz.dato) {
            raiz.izquierdo = eliminarNodo(raiz.izquierdo, dato);
        } else if (dato > raiz.dato) {
            raiz.derecho = eliminarNodo(raiz.derecho, dato);
        } else {
            if (raiz.izquierdo == null) {
                return raiz.derecho;
            } else if (raiz.derecho == null) {
                return raiz.izquierdo;
            } else {
                Nodo nodoPredecesor = raiz.izquierdo;
                while (nodoPredecesor.derecho != null) {
                    nodoPredecesor = nodoPredecesor.derecho;
                }
                raiz.dato = nodoPredecesor.dato;
                raiz.izquierdo = eliminarNodo(raiz.izquierdo, nodoPredecesor.dato);
            }
        }
        return raiz;
    }

    // Método recursivo para imprimir un árbol binario en orden
    public static void imprimirArbolInOrden(Nodo raiz) {
        if (raiz == null) {
            return;
        }
        imprimirArbolInOrden(raiz.izquierdo);
        System.out.print(raiz.dato + " ");
        imprimirArbolInOrden(raiz.derecho);
    }

    // Método recursivo para imprimir un árbol binario en preorden
    public static void imprimirArbolPreOrden(Nodo raiz) {
        if (raiz == null) {
            return;
        }
        System.out.print(raiz.dato + " ");
        imprimirArbolPreOrden(raiz.izquierdo);
        imprimirArbolPreOrden(raiz.derecho);
    }

    // Método recursivo para imprimir un árbol binario en postorden
    public static void imprimirArbolPostOrden(Nodo raiz) {
        if (raiz == null) {
            return;
        }
        imprimirArbolPostOrden(raiz.izquierdo);
        imprimirArbolPostOrden(raiz.derecho);
        System.out.print(raiz.dato + " ");
    }

    // Método para crear un árbol binario de búsqueda a partir de una lista de datos
    public static Nodo crearArbolBinarioBusqueda(List<Integer> datos) {
        Nodo raiz = null;
        for (int dato : datos) {
            agregarNodo(raiz, dato);
        }
        return raiz;
    }

    // Método para probar el código
    public static void main(String[] args) {
        List<Integer> datos = Arrays.asList(10, 5, 15, 2, 7, 12, 20);
        Nodo raiz = crearArbolBinarioBusqueda(datos);

        System.out.println("Árbol binario de búsqueda:");
        imprimirArbolInOrden(raiz);
        System.out.println();

        int datoABuscar = 15;
        boolean encontrado = buscarNodo(raiz, datoABuscar);
        if (encontrado) {
            System.out.println("El dato " + datoABuscar + " se encontró en el árbol binario de búsqueda.");
        } else {
            System.out.println("El dato " + datoABuscar + " no se encontró en el árbol binario de búsqueda.");
        }

        Nodo raizEliminada = eliminarNodo(raiz, 10);
        System.out.println("Árbol binario de búsqueda después de eliminar el dato 10:");
        imprimirArbolPreOrden(raizEliminada);
        System.out.println();
    }