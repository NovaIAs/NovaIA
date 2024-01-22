```d
import std.stdio, std.string, std.algorithm;

class Nodo {
    Nodo *esquerda, *direita;
    int dato;

    Nodo(int dato) {
        this.dato = dato;
        this.esquerda = null;
        this.direita = null;
    }
}

class ArvoreBinaria {
    Nodo *raiz;

    ArvoreBinaria() {
        this.raiz = null;
    }

    void inserir(int dato) {
        Nodo *nuevoNodo = new Nodo(dato);
        if (this.raiz == null) {
            this.raiz = nuevoNodo;
        } else {
            insertarRecursivo(nuevoNodo, this.raiz);
        }
    }

    void insertarRecursivo(Nodo *nuevoNodo, Nodo *nodoActual) {
        if (nuevoNodo->dato < nodoActual->dato) {
            if (nodoActual->esquerda == null) {
                nodoActual->esquerda = nuevoNodo;
            } else {
                insertarRecursivo(nuevoNodo, nodoActual->esquerda);
            }
        } else {
            if (nodoActual->direita == null) {
                nodoActual->direita = nuevoNodo;
            } else {
                insertarRecursivo(nuevoNodo, nodoActual->direita);
            }
        }
    }

    void percorrerEmOrdem() {
        percorrerEmOrdemRecursivo(this.raiz);
    }

    void percorrerEmOrdemRecursivo(Nodo *nodoActual) {
        if (nodoActual != null) {
            percorrerEmOrdemRecursivo(nodoActual->esquerda);
            System.write("{0} ", [nodoActual->dato]);
            percorrerEmOrdemRecursivo(nodoActual->direita);
        }
    }

    void percorrerPosOrdem() {
        percorrerPosOrdemRecursivo(this.raiz);
    }

    void percorrerPosOrdemRecursivo(Nodo *nodoActual) {
        if (nodoActual != null) {
            percorrerPosOrdemRecursivo(nodoActual->esquerda);
            percorrerPosOrdemRecursivo(nodoActual->direita);
            System.write("{0} ", [nodoActual->dato]);
        }
    }

    void percorrerPreOrdem() {
        percorrerPreOrdemRecursivo(this.raiz);
    }

    void percorrerPreOrdemRecursivo(Nodo *nodoActual) {
        if (nodoActual != null) {
            System.write("{0} ", [nodoActual->dato]);
            percorrerPreOrdemRecursivo(nodoActual->esquerda);
            percorrerPreOrdemRecursivo(nodoActual->direita);
        }
    }

    bool procurar(int dato) {
        return procurarRecursivo(this.raiz, dato);
    }

    bool procurarRecursivo(Nodo *nodoActual, int dato) {
        if (nodoActual == null) {
            return false;
        } else if (nodoActual->dato == dato) {
            return true;
        } else if (dato < nodoActual->dato) {
            return procurarRecursivo(nodoActual->esquerda, dato);
        } else {
            return procurarRecursivo(nodoActual->direita, dato);
        }
    }

    int altura() {
        return alturaRecursiva(this.raiz);
    }

    int alturaRecursiva(Nodo *nodoActual) {
        if (nodoActual == null) {
            return -1;
        } else {
            int alturaEsquerda = alturaRecursiva(nodoActual->esquerda);
            int alturaDireita = alturaRecursiva(nodoActual->direita);
            return max(alturaEsquerda, alturaDireita) + 1;
        }
    }

    int contaNodos() {
        return contaNodosRecursivo(this.raiz);
    }

    int contaNodosRecursivo(Nodo *nodoActual) {
        if (nodoActual == null) {
            return 0;
        } else {
            int contaEsquerda = contaNodosRecursivo(nodoActual->esquerda);
            int contaDireita = contaNodosRecursivo(nodoActual->direita);
            return contaEsquerda + contaDireita + 1;
        }
    }

    int contaFolhas() {
        return contaFolhasRecursivo(this.raiz);
    }

    int contaFolhasRecursivo(Nodo *nodoActual) {
        if (nodoActual == null) {
            return 0;
        } else if (nodoActual->esquerda == null && nodoActual->direita == null) {
            return 1;
        } else {
            int contaEsquerda = contaFolhasRecursivo(nodoActual->esquerda);
            int contaDireita = contaFolhasRecursivo(nodoActual->direita);
            return contaFolhasRecursivo(nodoActual->esquerda) + contaFolhasRecursivo(nodoActual->direita);
        }
    }

    int minimo() {
        return minimoRecursivo(this.raiz);
    }

    int minimoRecursivo(Nodo *nodoActual) {
        if (nodoActual->esquerda == null) {
            return nodoActual->dato;
        } else {
            return minimoRecursivo(nodoActual->esquerda);
        }
    }

    int maximo() {
        return maximoRecursivo(this.raiz);
    }

    int maximoRecursivo(Nodo *nodoActual) {
        if (nodoActual->direita == null) {
            return nodoActual->dato;
        } else {
            return maximoRecursivo(nodoActual->direita);
        }
    }

    void eliminar(int dato) {
        eliminarRecursivo(this.raiz, dato);
    }

    Nodo *eliminarRecursivo(Nodo *nodoActual, int dato) {
        if (nodoActual == null) {
            return null;
        } else if (dato < nodoActual->dato) {
            nodoActual->esquerda = eliminarRecursivo(nodoActual->esquerda, dato);
        } else if (dato > nodoActual->dato) {
            nodoActual->direita = eliminarRecursivo(nodoActual->direita, dato);
        } else {
            if (nodoActual->esquerda == null) {
                return nodoActual->direita;
            } else if (nodoActual->direita == null) {
                return nodoActual->esquerda;
            } else {
                Nodo *nodoMinimo = encontrarMinimo(nodoActual->direita);
                nodoActual->dato = nodoMinimo->dato;
                nodoActual->direita = eliminarRecursivo(nodoActual->direita, nodoMinimo->dato);
            }
        }
        return nodoActual;
    }

    Nodo *encontrarMinimo(Nodo *nodoActual) {
        if (nodoActual == null) {
            return null;
        } else if (nodoActual->esquerda == null) {
            return nodoActual;
        } else {
            return encontrarMinimo(nodoActual->esquerda);
        }
    }
}

void main() {
    ArvoreBinaria arvoreBinaria;

    arvoreBinaria = new ArvoreBinaria();

    arvoreBinaria.inserir(10);
    arvoreBinaria.inserir(5);
    arvoreBinaria.inserir(15);
    arvoreBinaria.inserir(2);
    arvoreBinaria.inserir(7);
    arvoreBinaria.inserir(12);
    arvoreBinaria.inserir(20);

    System.write("Percorrer em ordem: ");
    arvoreBinaria.percorrerEmOrdem();
    System.write("\n");

    System.write("Percorrer em pos-ordem: ");
    arvoreBinaria.percorrerPosOrdem();
    System.write("\n");

    System.write("Percorrer em pre-ordem: ");
    arvoreBinaria.percorrerPreOrdem();
    System.write("\n");

    System.write("Procurar o número 12: {0}\n", [arvoreBinaria.procurar(12)]);
    System.write("Procurar o número 25: {0}\n", [arvoreBinaria.procurar(25)]);

    System.write("Altura da árvore: {0}\n", [arvoreBinaria.altura()]);

    System.write("Número de nós da árvore: {0}\n", [arvoreBinaria.contaNodos()]);

    System.write("Número de folhas da árvore: {0}\n", [arvoreBinaria.contaFolhas()]);

    System.write("Valor mínimo da árvore: {0}\n", [arvoreBinaria.minimo()]);

    System.write("Valor máximo da árvore: {0}\n", [arvoreBinaria.maximo()]);

    arvoreBinaria.eliminar(10);

    System.write("Percorrer em ordem após eliminar o número 10: ");
    arvoreBinaria.percorrerEmOrdem();
    System.write("\n");
}
Explicación del código:

**Inserción de nodos:**

La función `insertar` inserta un nuevo nodo en el árbol binario. Si el árbol está vacío, el nuevo nodo se convierte en la raíz del árbol. De lo contrario, la función llama a la función `insertarRecursivo` para insertar el nuevo nodo en la ubicación correcta.

La función `insertarRecursivo` inserta el nuevo nodo en el árbol binario de forma recursiva. Si el dato del nuevo nodo es menor que el dato del nodo actual, la función llama a `insertarRecursivo` en el hijo izquierdo del nodo actual. De lo contrario, la función llama a `insertarRecursivo` en el hijo derecho del nodo actual.

**Recorridos del árbol:**

El árbol binario se puede recorrer de tres maneras diferentes:

* Recorrido en orden: Este recorrido visita los nodos del árbol en orden creciente de datos.
* Recorrido en posorden: Este recorrido visita los nodos del árbol en orden decreciente de datos.
* Recorrido en preorden: Este recorrido visita el nodo actual, luego su hijo izquierdo y finalmente su hijo derecho.

Las funciones `percorrerEmOrdem`, `percorrerPosOrdem` y `percorrerPreOrdem` implementan estos tres recorridos, respectivamente.

**Búsqueda de nodos:**

La función `procurar` busca un nodo con un dato dado en el árbol binario. La función llama a la función `procurarRecursivo` para realizar la búsqueda de forma recursiva.

La función `procurarRecursivo` busca el nodo con el dato dado en el árbol binario de forma recursiva. Si el nodo actual es nulo, la función devuelve falso. Si el dato del nodo actual es igual al dato dado, la función devuelve verdadero. De lo contrario, la función llama a `procurarRecursivo` en el hijo izquierdo o derecho del nodo actual, dependiendo de si el dato dado es menor o mayor que el dato del nodo actual.

**Altura del árbol:**

La función `altura` calcula la altura del árbol binario. La función llama a la función `alturaRecursiva` para calcular la altura de forma recursiva.

La función `alturaRecursiva` calcula la altura del árbol binario de forma recursiva. Si el nodo actual es nulo, la función devuelve -1. De lo contrario, la función calcula la altura del subárbol izquierdo y la altura del subárbol derecho del nodo actual. La altura del árbol binario es la altura máxima de los subárboles izquierdo y derecho del nodo actual más uno.

**Número de nodos del árbol:**

La función `contaNodos` cuenta el número de nodos del árbol binario. La función llama a la función `contaNodosRecursivo` para contar los nodos de forma recursiva.

La función `contaNodosRecursivo` cuenta el número de nodos del árbol binario de forma recursiva. Si el nodo actual es nulo, la función devuelve 0. De lo contrario, la función cuenta los nodos del subárbol izquierdo y los nodos del subárbol derecho del nodo actual. El número de nodos del árbol binario es el número de nodos del subárbol izquierdo más el número de nodos del subárbol derecho del nodo actual más uno.

**Número de hojas del árbol:**

La función `contaFolhas` cuenta el número de hojas del árbol binario. La función llama a la función `contaFolhasRecursivo` para contar las hojas de forma recursiva.

La función `contaFolhasRecursivo` cuenta el número de hojas del árbol binario de forma recursiva. Si el nodo actual es nulo, la función devuelve 0. Si el nodo actual es una hoja, la función devuelve 1. De lo contrario, la función cuenta las hojas del subárbol izquierdo y las hojas del subárbol derecho del nodo actual. El número de hojas del árbol binario es el número de hojas del subárbol izquierdo más el número de hojas del subárbol derecho del nodo actual.

**Valor mínimo del árbol:**

La función `minimo` calcula el valor mínimo del árbol binario. La función llama a la función `minimoRecursivo` para calcular el valor mínimo de forma recursiva.

La función `minimoRecursivo` calcula el valor mínimo del árbol binario de forma recursiva. Si el nodo actual es nulo, la función devuelve nulo. Si el nodo actual no tiene hijo izquierdo, la función devuelve el dato del nodo actual. De lo contrario, la función llama a `minimoRecursivo` en el hijo izquierdo del nodo actual.

**Valor máximo del árbol:**

La función `maximo` calcula el valor máximo del árbol binario. La función llama a la función `maximoRecursivo` para calcular el valor máximo de forma recursiva.

La función `maximoRecursivo` calcula el valor máximo del árbol binario de forma recursiva. Si el nodo actual es nulo, la función devuelve nulo. Si el nodo actual no tiene hijo derecho, la función devuelve el dato del nodo actual. De lo contrario, la función llama a `maximoRecursivo` en el hijo derecho del nodo actual.

**Eliminación de nodos:**

La función `eliminar` elimina un nodo con un dato dado del árbol binario. La función llama a la función `eliminarRecursivo` para eliminar el nodo de forma recursiva.

La función `eliminarRecursivo` elimina el nodo con el dato dado del árbol binario de forma recursiva. Si el nodo actual es nulo, la función devuelve nulo. Si el dato del nodo actual es igual al dato dado, la función elimina el nodo actual y devuelve el nodo hijo izquierdo o derecho del nodo actual, dependiendo de cuál esté presente. De lo contrario, la función llama a `eliminarRecursivo` en el hijo izquierdo o derecho del nodo actual, dependiendo de si el dato dado es menor o mayor que el dato del nodo actual.

**Función `encontrarMinimo`:**

La función `encontrarMinimo` encuentra el nodo con el valor mínimo en el árbol binario. La función llama a la función `encontrarMinimoRecursivo` para encontrar el nodo con el valor mínimo de forma recursiva.

La función `encontrarMinimoRecursivo` encuentra el nodo con el valor mínimo en el árbol binario de forma recursiva. Si el nodo actual es nulo, la función devuelve nulo. Si el nodo actual no tiene hijo izquierdo, la función devuelve el nodo actual. De lo contrario, la función llama a `encontrarMinimoRecursivo` en el hijo izquierdo del nodo actual.

**Función `main`:**

La función `main` es la función de entrada del programa. La función crea un objeto de la clase `ArvoreBinaria` y llama a los métodos del objeto para insertar, recorrer, buscar, eliminar y encontrar el valor mínimo y máximo del árbol binario.