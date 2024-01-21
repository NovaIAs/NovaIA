```java

import java.util.ArrayList;
import java.util.List;
import java.util.Arrays;

public class CodigoComplejo {

    // Clase interna para representar un nodo de un árbol binario de búsqueda
    private static class NodoArbol {
        int valor;
        NodoArbol izquierdo;
        NodoArbol derecho;

        public NodoArbol(int valor) {
            this.valor = valor;
            this.izquierdo = null;
            this.derecho = null;
        }
    }

    // Función recursiva para insertar un valor en un árbol binario de búsqueda
    private static NodoArbol insertarArbol(NodoArbol raiz, int valor) {
        if (raiz == null) {
            return new NodoArbol(valor);
        }

        if (valor < raiz.valor) {
            raiz.izquierdo = insertarArbol(raiz.izquierdo, valor);
        } else if (valor > raiz.valor) {
            raiz.derecho = insertarArbol(raiz.derecho, valor);
        }

        return raiz;
    }

    // Función recursiva para buscar un valor en un árbol binario de búsqueda
    private static boolean buscarArbol(NodoArbol raiz, int valor) {
        if (raiz == null) {
            return false;
        }

        if (valor == raiz.valor) {
            return true;
        }

        if (valor < raiz.valor) {
            return buscarArbol(raiz.izquierdo, valor);
        } else {
            return buscarArbol(raiz.derecho, valor);
        }
    }

    // Función recursiva para imprimir los valores de un árbol binario de búsqueda en orden ascendente
    private static void imprimirArbolAscendente(NodoArbol raiz) {
        if (raiz == null) {
            return;
        }

        imprimirArbolAscendente(raiz.izquierdo);
        System.out.println(raiz.valor);
        imprimirArbolAscendente(raiz.derecho);
    }

    // Función recursiva para imprimir los valores de un árbol binario de búsqueda en orden descendente
    private static void imprimirArbolDescendente(NodoArbol raiz) {
        if (raiz == null) {
            return;
        }

        imprimirArbolDescendente(raiz.derecho);
        System.out.println(raiz.valor);
        imprimirArbolDescendente(raiz.izquierdo);
    }

    // Función recursiva para eliminar un valor de un árbol binario de búsqueda
    private static NodoArbol eliminarArbol(NodoArbol raiz, int valor) {
        if (raiz == null) {
            return null;
        }

        if (valor == raiz.valor) {
            // Caso 1: Nodo hoja
            if (raiz.izquierdo == null && raiz.derecho == null) {
                return null;
            }

            // Caso 2: Nodo con un hijo
            if (raiz.izquierdo == null) {
                return raiz.derecho;
            } else if (raiz.derecho == null) {
                return raiz.izquierdo;
            }

            // Caso 3: Nodo con dos hijos
            NodoArbol minDerecho = raiz.derecho;
            while (minDerecho.izquierdo != null) {
                minDerecho = minDerecho.izquierdo;
            }

            raiz.valor = minDerecho.valor;
            raiz.derecho = eliminarArbol(raiz.derecho, minDerecho.valor);

            return raiz;
        }

        if (valor < raiz.valor) {
            raiz.izquierdo = eliminarArbol(raiz.izquierdo, valor);
        } else {
            raiz.derecho = eliminarArbol(raiz.derecho, valor);
        }

        return raiz;
    }

    // Función recursiva para calcular la altura de un árbol binario
    private static int calcularAlturaArbol(NodoArbol raiz) {
        if (raiz == null) {
            return 0;
        }

        int alturaIzquierda = calcularAlturaArbol(raiz.izquierdo);
        int alturaDerecha = calcularAlturaArbol(raiz.derecho);

        return Math.max(alturaIzquierda, alturaDerecha) + 1;
    }

    // Función recursiva para calcular el número de nodos de un árbol binario
    private static int calcularNumeroNodosArbol(NodoArbol raiz) {
        if (raiz == null) {
            return 0;
        }

        int numeroNodosIzquierda = calcularNumeroNodosArbol(raiz.izquierdo);
        int numeroNodosDerecha = calcularNumeroNodosArbol(raiz.derecho);

        return numeroNodosIzquierda + numeroNodosDerecha + 1;
    }

    // Función recursiva para calcular la suma de los valores de los nodos de un árbol binario
    private static int calcularSumaValoresArbol(NodoArbol raiz) {
        if (raiz == null) {
            return 0;
        }

        int sumaValoresIzquierda = calcularSumaValoresArbol(raiz.izquierdo);
        int sumaValoresDerecha = calcularSumaValoresArbol(raiz.derecho);

        return sumaValoresIzquierda + sumaValoresDerecha + raiz.valor;
    }

    // Función recursiva para calcular el valor promedio de los nodos de un árbol binario
    private static double calcularPromedioValoresArbol(NodoArbol raiz) {
        if (raiz == null) {
            return 0;
        }

        int sumaValores = calcularSumaValoresArbol(raiz);
        int numeroNodos = calcularNumeroNodosArbol(raiz);

        return (double) sumaValores / numeroNodos;
    }

    public static void main(String[] args) {
        // Crear un árbol binario de búsqueda
        NodoArbol raiz = new NodoArbol(50);
        raiz = insertarArbol(raiz, 30);
        raiz = insertarArbol(raiz, 70);
        raiz = insertarArbol(raiz, 20);
        raiz = insertarArbol(raiz, 40);
        raiz = insertarArbol(raiz, 60);
        raiz = insertarArbol(raiz, 80);

        // Imprimir el árbol en orden ascendente
        System.out.println("Árbol en orden ascendente:");
        imprimirArbolAscendente(raiz);

        // Imprimir el árbol en orden descendente
        System.out.println("Árbol en orden descendente:");
        imprimirArbolDescendente(raiz);

        // Buscar un valor en el árbol
        System.out.println("¿Existe el valor 40 en el árbol?");
        System.out.println(buscarArbol(raiz, 40));

        // Eliminar un valor del árbol
        raiz = eliminarArbol(raiz, 20);

        // Imprimir el árbol después de eliminar el valor 20
        System.out.println("Árbol después de eliminar el valor 20:");
        imprimirArbolAscendente(raiz);

        // Calcular la altura del árbol
        System.out.println("Altura del árbol:");
        System.out.println(calcularAlturaArbol(raiz));

        // Calcular el número de nodos del árbol
        System.out.println("Número de nodos del árbol:");
        System.out.println(calcularNumeroNodosArbol(raiz));

        // Calcular la suma de los valores de los nodos del árbol
        System.out.println("Suma de los valores de los nodos del árbol:");
        System.out.println(calcularSumaValores