```java
import java.util.*;
import java.io.*;

public class CodigoComplejo {

    // Clase interna para representar un nodo de un árbol binario de búsqueda
    private static class Nodo {
        int valor;
        Nodo izquierdo;
        Nodo derecho;

        public Nodo(int valor) {
            this.valor = valor;
            this.izquierdo = null;
            this.derecho = null;
        }
    }

    // Método para insertar un valor en un árbol binario de búsqueda
    public static Nodo insertar(Nodo raiz, int valor) {
        if (raiz == null) {
            return new Nodo(valor);
        } else if (valor < raiz.valor) {
            raiz.izquierdo = insertar(raiz.izquierdo, valor);
        } else {
            raiz.derecho = insertar(raiz.derecho, valor);
        }

        return raiz;
    }

    // Método para buscar un valor en un árbol binario de búsqueda
    public static boolean buscar(Nodo raiz, int valor) {
        if (raiz == null) {
            return false;
        } else if (valor == raiz.valor) {
            return true;
        } else if (valor < raiz.valor) {
            return buscar(raiz.izquierdo, valor);
        } else {
            return buscar(raiz.derecho, valor);
        }
    }

    // Método para eliminar un valor de un árbol binario de búsqueda
    public static Nodo eliminar(Nodo raiz, int valor) {
        if (raiz == null) {
            return null;
        } else if (valor == raiz.valor) {
            if (raiz.izquierdo == null && raiz.derecho == null) {
                return null;
            } else if (raiz.izquierdo == null) {
                return raiz.derecho;
            } else if (raiz.derecho == null) {
                return raiz.izquierdo;
            } else {
                Nodo sucesor = raiz.derecho;
                while (sucesor.izquierdo != null) {
                    sucesor = sucesor.izquierdo;
                }

                raiz.valor = sucesor.valor;
                raiz.derecho = eliminar(raiz.derecho, sucesor.valor);
            }
        } else if (valor < raiz.valor) {
            raiz.izquierdo = eliminar(raiz.izquierdo, valor);
        } else {
            raiz.derecho = eliminar(raiz.derecho, valor);
        }

        return raiz;
    }

    // Método para imprimir un árbol binario de búsqueda en orden
    public static void imprimirEnOrden(Nodo raiz) {
        if (raiz == null) {
            return;
        }

        imprimirEnOrden(raiz.izquierdo);
        System.out.print(raiz.valor + " ");
        imprimirEnOrden(raiz.derecho);
    }

    // Método para imprimir un árbol binario de búsqueda en preorden
    public static void imprimirEnPreOrden(Nodo raiz) {
        if (raiz == null) {
            return;
        }

        System.out.print(raiz.valor + " ");
        imprimirEnPreOrden(raiz.izquierdo);
        imprimirEnPreOrden(raiz.derecho);
    }

    // Método para imprimir un árbol binario de búsqueda en postorden
    public static void imprimirEnPostOrden(Nodo raiz) {
        if (raiz == null) {
            return;
        }

        imprimirEnPostOrden(raiz.izquierdo);
        imprimirEnPostOrden(raiz.derecho);
        System.out.print(raiz.valor + " ");
    }

    // Método para contar el número de hojas en un árbol binario
    public static int contarHojas(Nodo raiz) {
        if (raiz == null) {
            return 0;
        } else if (raiz.izquierdo == null && raiz.derecho == null) {
            return 1;
        } else {
            return contarHojas(raiz.izquierdo) + contarHojas(raiz.derecho);
        }
    }

    // Método para encontrar el valor máximo en un árbol binario
    public static int encontrarMaximo(Nodo raiz) {
        if (raiz == null) {
            return Integer.MIN_VALUE;