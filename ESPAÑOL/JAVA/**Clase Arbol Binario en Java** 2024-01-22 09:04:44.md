```java
public class CodigoComplejo {

    // Definimos una interfaz para representar un nodo en un árbol binario.
    interface NodoArbol {
        int getValor();
        NodoArbol getIzquierdo();
        NodoArbol getDerecho();
    }

    // Implementamos una clase concreta para representar un nodo en un árbol binario.
    class NodoArbolBinario implements NodoArbol {
        private int valor;
        private NodoArbolBinario izquierdo;
        private NodoArbolBinario derecho;

        public NodoArbolBinario(int valor) {
            this.valor = valor;
            this.izquierdo = null;
            this.derecho = null;
        }

        @Override
        public int getValor() {
            return valor;
        }

        @Override
        public NodoArbolBinario getIzquierdo() {
            return izquierdo;
        }

        @Override
        public NodoArbolBinario getDerecho() {
            return derecho;
        }

        public void setIzquierdo(NodoArbolBinario izquierdo) {
            this.izquierdo = izquierdo;
        }

        public void setDerecho(NodoArbolBinario derecho) {
            this.derecho = derecho;
        }
    }

    // Definimos una clase para representar un árbol binario.
    class ArbolBinario {
        private NodoArbolBinario raiz;

        public ArbolBinario() {
            this.raiz = null;
        }

        public NodoArbolBinario getRaiz() {
            return raiz;
        }

        public void setRaiz(NodoArbolBinario raiz) {
            this.raiz = raiz;
        }

        // Método para insertar un nuevo nodo en el árbol binario.
        public void insertar(int valor) {
            NodoArbolBinario nuevoNodo = new NodoArbolBinario(valor);

            if (raiz == null) {
                raiz = nuevoNodo;
            } else {
                insertarRecursivo(nuevoNodo, raiz);
            }
        }

        // Método recursivo para insertar un nuevo nodo en el árbol binario.
        private void insertarRecursivo(NodoArbolBinario nuevoNodo, NodoArbolBinario nodoActual) {
            if (nuevoNodo.getValor() < nodoActual.getValor()) {
                if (nodoActual.getIzquierdo() == null) {
                    nodoActual.setIzquierdo(nuevoNodo);
                } else {
                    insertarRecursivo(nuevoNodo, nodoActual.getIzquierdo());
                }
            } else {
                if (nodoActual.getDerecho() == null) {
                    nodoActual.setDerecho(nuevoNodo);
                } else {
                    insertarRecursivo(nuevoNodo, nodoActual.getDerecho());
                }
            }
        }

        // Método para buscar un nodo en el árbol binario.
        public NodoArbolBinario buscar(int valor) {
            if (raiz == null) {
                return null;
            } else {
                return buscarRecursivo(valor, raiz);
            }
        }

        // Método recursivo para buscar un nodo en el árbol binario.
        private NodoArbolBinario buscarRecursivo(int valor, NodoArbolBinario nodoActual) {
            if (nodoActual == null) {
                return null;
            } else if (nodoActual.getValor() == valor) {
                return nodoActual;
            } else if (valor < nodoActual.getValor()) {
                return buscarRecursivo(valor, nodoActual.getIzquierdo());
            } else {
                return buscarRecursivo(valor, nodoActual.getDerecho());
            }
        }

        // Método para eliminar un nodo del árbol binario.
        public void eliminar(int valor) {
            if (raiz == null) {
                return;
            } else {
                raiz = eliminarRecursivo(valor, raiz);
            }
        }

        // Método recursivo para eliminar un nodo del árbol binario.
        private NodoArbolBinario eliminarRecursivo(int valor, NodoArbolBinario nodoActual) {
            if (nodoActual == null) {
                return null;
            } else if (valor < nodoActual.getValor()) {
                nodoActual.setIzquierdo(eliminarRecursivo(valor, nodoActual.getIzquierdo()));
            } else if (valor > nodoActual.getValor()) {
                nodoActual.setDerecho(eliminarRecursivo(valor, nodoActual.getDerecho()));
            } else {
                if (nodoActual.getIzquierdo() == null && nodoActual.getDerecho() == null) {
                    return null;
                } else if (nodoActual.getIzquierdo() == null) {
                    return nodoActual.getDerecho();
                } else if (nodoActual.getDerecho() == null) {
                    return nodoActual.getIzquierdo();
                } else {
                    NodoArbolBinario nodoSucesor = encontrarSucesor(nodoActual.getDerecho());
                    nodoActual.setValor(nodoSucesor.getValor());
                    nodoActual.setDerecho(eliminarRecursivo(nodoSucesor.getValor(), nodoActual.getDerecho()));
                }
            }

            return nodoActual;
        }

        // Método para encontrar el nodo sucesor de un nodo dado.
        private NodoArbolBinario encontrarSucesor(NodoArbolBinario nodoActual) {
            if (nodoActual.getIzquierdo() == null) {
                return nodoActual;
            } else {
                return encontrarSucesor(nodoActual.getIzquierdo());
            }
        }

        // Método para imprimir el árbol binario en forma de preorden.
        public void imprimirPreorden() {
            imprimirPreordenRecursivo(raiz);
        }

        // Método recursivo para imprimir el árbol binario en forma de preorden.
        private void imprimirPreordenRecursivo(NodoArbolBinario nodoActual) {
            if (nodoActual == null) {
                return;
            }

            System.out.print(nodoActual.getValor() + " ");
            imprimirPreordenRecursivo(nodoActual.getIzquierdo());
            imprimirPreordenRecursivo(nodoActual.getDerecho());
        }

        // Método para imprimir el árbol binario en forma de inorden.
        public void imprimirInorden() {
            imprimirInordenRecursivo(raiz);
        }

        // Método recursivo para imprimir el árbol binario en forma de inorden.
        private void imprimirInordenRecursivo(NodoArbolBinario nodoActual) {
            if (nodoActual == null) {
                return;
            }

            imprimirInordenRecursivo(nodoActual.getIzquierdo());
            System.out.print(nodoActual.getValor() + " ");
            imprimirInordenRecursivo(nodoActual.getDerecho());
        }

        // Método para imprimir el árbol binario en forma de postorden.
        public void imprimirPostorden() {
            imprimirPostordenRecursivo(raiz);
        }

        // Método recursivo para imprimir el árbol binario en forma de postorden.
        private void imprimirPostordenRecursivo(NodoArbolBinario nodoActual) {
            if (nodoActual == null) {
                return;
            }

            imprimirPostordenRecursivo(nodoActual.getIzquierdo());
            imprimirPostordenRecursivo(nodoActual.getDerecho());
            System.out.print(nodoActual.getValor() + " ");
        }
    }

    public static void main(String[] args) {
        // Creamos un árbol binario.
        ArbolBinario arbol = new ArbolBinario();

        // Insertamos algunos nodos en el árbol.
        arbol.insertar(10);
        arbol.insertar(5);
        arbol.insertar(15);
        arbol.insertar(2);
        arbol.insertar(7);
        arbol.insertar(12);
        arbol.insertar(20);

        // Imprimimos el árbol en forma de preorden.
        System.out.println("Preorden:");
        arbol.imprimirPreorden();

        // Imprimimos el árbol en forma de inorden.
        System.out.println("\nInorden:");
        arbol.imprimirInorden();

        // Imprimimos el árbol en forma de postorden.
        System.out.println("\nPostorden:");
        arbol.imprimirPostorden();

        // Buscamos un nodo en el árbol.
        NodoArbolBinario nodoBuscado = arbol.buscar(12);

        // Imprimimos el nodo buscado.
        System.out.println("\nNodo buscado:");
        System.out.println(nodoBuscado.getValor());

        // Eliminamos un nodo del árbol.
        arbol.eliminar(15);

        // Imprimimos el árbol en forma de preorden después de eliminar el nodo.
        System.out.println("\nPreorden después de eliminar el nodo:");
        arbol.imprimirPreorden();
    }
}

Explicación del código:

* Definimos una interfaz llamada `NodoArbol` que representa un nodo en un árbol binario. Esta interfaz tiene tres métodos: `getValor()`, `getIzquierdo()` y `getDerecho()`. El método `getValor()` devuelve el valor del nodo, el método `getIzquierdo()` devuelve el nodo izquierdo del nodo actual y el método `getDerecho()` devuelve el nodo derecho del nodo actual.
* Definimos una clase llamada `NodoArbolBinario` que implementa la interfaz `NodoArbol`. Esta clase representa un nodo en un árbol binario. La clase tiene tres variables de instancia: `valor`, `izquierdo` y `derecho`. Las variables `valor` e `izquierdo` son de tipo `int`, mientras que la variable `derecho` es de tipo `NodoArbolBinario`. La variable `valor` almacena el valor del nodo, la variable `izquierdo` almacena el nodo izquierdo del nodo actual y la variable `derecho` almacena el nodo derecho del nodo actual. También definimos tres métodos de acceso para las variables de instancia: `getValor()`, `setValor()`, `getIzquierdo()`, `setIzquierdo()`, `getDerecho()` y `setDerecho()`.
* Definimos una clase llamada `ArbolBinario` que representa un árbol binario. Esta clase tiene una variable de instancia llamada `raiz` que es de tipo `NodoArbolBinario`. La variable `raiz` almacena el nodo raíz del árbol binario. También definimos varios métodos para insertar, buscar y eliminar nodos del árbol binario.
* En el método `main()`, creamos un árbol binario e insertamos algunos nodos en él. También imprimimos el árbol en forma de preorden, inorden y postorden. Buscamos un nodo en el árbol y lo eliminamos. Finalmente, imprimimos el árbol en forma de preorden después de eliminar el nodo.