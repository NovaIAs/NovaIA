```java
// Definición de la clase principal
public class CodigoComplejo {

    // Método principal
    public static void main(String[] args) {
        // Creación de un objeto de la clase Árbol Binario de Búsqueda
        ArbolBinarioBusqueda<Integer> arbol = new ArbolBinarioBusqueda<>();

        // Inserción de valores en el árbol
        arbol.insertar(10);
        arbol.insertar(5);
        arbol.insertar(15);
        arbol.insertar(2);
        arbol.insertar(7);
        arbol.insertar(12);
        arbol.insertar(20);

        // Impresión del árbol en preorden
        System.out.println("Impresión del árbol en preorden:");
        arbol.recorrerPreorden();

        // Impresión del árbol en orden
        System.out.println("Impresión del árbol en orden:");
        arbol.recorrerInorden();

        // Impresión del árbol en postorden
        System.out.println("Impresión del árbol en postorden:");
        arbol.recorrerPostorden();

        // Búsqueda de un valor en el árbol
        int valorABuscar = 12;
        System.out.println("Búsqueda del valor " + valorABuscar + ":");
        if (arbol.buscar(valorABuscar)) {
            System.out.println("El valor " + valorABuscar + " se encuentra en el árbol.");
        } else {
            System.out.println("El valor " + valorABuscar + " no se encuentra en el árbol.");
        }

        // Eliminación de un valor del árbol
        int valorAEliminar = 5;
        System.out.println("Eliminación del valor " + valorAEliminar + ":");
        arbol.eliminar(valorAEliminar);
        System.out.println("Impresión del árbol después de la eliminación:");
        arbol.recorrerInorden();

        // Cálculo de la altura del árbol
        int altura = arbol.calcularAltura();
        System.out.println("Altura del árbol: " + altura);

        // Cálculo del número de hojas del árbol
        int numeroDeHojas = arbol.calcularNumeroDeHojas();
        System.out.println("Número de hojas del árbol: " + numeroDeHojas);

        // Cálculo del número de nodos internos del árbol
        int numeroDeNodosInternos = arbol.calcularNumeroDeNodosInternos();
        System.out.println("Número de nodos internos del árbol: " + numeroDeNodosInternos);

        // Cálculo del grado promedio de los nodos del árbol
        double gradoPromedio = arbol.calcularGradoPromedio();
        System.out.println("Grado promedio de los nodos del árbol: " + gradoPromedio);
    }

    // Definición de la clase Árbol Binario de Búsqueda
    private static class ArbolBinarioBusqueda<T extends Comparable<T>> {

        // Nodo raíz del árbol
        private Nodo<T> raiz;

        // Clase Nodo
        private static class Nodo<T> {

            // Valor del nodo
            private T valor;

            // Nodo izquierdo
            private Nodo<T> izquierdo;

            // Nodo derecho
            private Nodo<T> derecho;

            // Constructor
            public Nodo(T valor) {
                this.valor = valor;
            }
        }

        // Métodos de la clase Árbol Binario de Búsqueda

        // Método para insertar un valor en el árbol
        public void insertar(T valor) {
            Nodo<T> nuevoNodo = new Nodo<>(valor);
            if (raiz == null) {
                raiz = nuevoNodo;
            } else {
                insertarRecursivo(nuevoNodo, raiz);
            }
        }

        // Método recursivo para insertar un valor en el árbol
        private void insertarRecursivo(Nodo<T> nuevoNodo, Nodo<T> actual) {
            if (nuevoNodo.valor.compareTo(actual.valor) < 0) {
                if (actual.izquierdo == null) {
                    actual.izquierdo = nuevoNodo;
                } else {
                    insertarRecursivo(nuevoNodo, actual.izquierdo);
                }
            } else {
                if (actual.derecho == null) {
                    actual.derecho = nuevoNodo;
                } else {
                    insertarRecursivo(nuevoNodo, actual.derecho);
                }
            }
        }

        // Método para eliminar un valor del árbol
        public void eliminar(T valor) {
            raiz = eliminarRecursivo(valor, raiz);
        }

        // Método recursivo para eliminar un valor del árbol
        private Nodo<T> eliminarRecursivo(T valor, Nodo<T> actual) {
            if (actual == null) {
                return null;
            }

            if (valor.compareTo(actual.valor) < 0) {
                actual.izquierdo = eliminarRecursivo(valor, actual.izquierdo);
                return actual;
            } else if (valor.compareTo(actual.valor) > 0) {
                actual.derecho = eliminarRecursivo(valor, actual.derecho);
                return actual;
            } else {
                if (actual.izquierdo == null) {
                    return actual.derecho;
                } else if (actual.derecho == null) {
                    return actual.izquierdo;
                } else {
                    actual.valor = encontrarSucesor(actual.derecho);
                    actual.derecho = eliminarRecursivo(actual.valor, actual.derecho);
                    return actual;
                }
            }
        }

        // Método para encontrar el sucesor de un nodo
        private T encontrarSucesor(Nodo<T> actual) {
            if (actual.izquierdo == null) {
                return actual.valor;
            } else {
                return encontrarSucesor(actual.izquierdo);
            }
        }

        // Método para buscar un valor en el árbol
        public boolean buscar(T valor) {
            return buscarRecursivo(valor, raiz);
        }

        // Método recursivo para buscar un valor en el árbol
        private boolean buscarRecursivo(T valor, Nodo<T> actual) {