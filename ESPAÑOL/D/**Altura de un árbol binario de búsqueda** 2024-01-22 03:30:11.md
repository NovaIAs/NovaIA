```d
import std.stdio, std.string, std.range, std.algorithm, std.conv, std.traits;

class NodoArbolBinario {
    T data;
    NodoArbolBinario!T? left;
    NodoArbolBinario!T? right;

    NodoArbolBinario(T data) {
        this.data = data;
    }

    void insertar(T data) {
        if (this.data > data) {
            if (!this.left) {
                this.left = new NodoArbolBinario!(data);
            } else {
                this.left.insertar(data);
            }
        } else {
            if (!this.right) {
                this.right = new NodoArbolBinario!(data);
            } else {
                this.right.insertar(data);
            }
        }
    }

    int altura() {
        return Math.max(this.left ? this.left.altura() : 0,
                        this.right ? this.right.altura() : 0) + 1;
    }

    void imprimir() {
        if (this.left) {
            this.left.imprimir();
        }
        printl(this.data);
        if (this.right) {
            this.right.imprimir();
        }
    }
}

void main() {
    auto arbol = new NodoArbolBinario!(10);
    arbol.insertar(5);
    arbol.insertar(15);
    arbol.insertar(3);
    arbol.insertar(7);
    arbol.insertar(12);
    arbol.insertar(20);

    printl("Altura del árbol:", arbol.altura());
    printl("Impresión del árbol:");
    arbol.imprimir();
}
```

Explicación del código:

1. Definimos una clase `NodoArbolBinario` que representa un nodo en un árbol binario. Esta clase tiene tres propiedades:

    * `data`: El dato almacenado en el nodo.
    * `left`: Un puntero al nodo hijo izquierdo.
    * `right`: Un puntero al nodo hijo derecho.

2. La clase `NodoArbolBinario` tiene un constructor que recibe un dato y lo asigna a la propiedad `data`.

3. La clase `NodoArbolBinario` tiene un método `insertar` que recibe un dato y lo inserta en el árbol binario. El método compara el dato con el dato almacenado en el nodo actual y, si es menor, lo inserta en el nodo hijo izquierdo. Si es mayor, lo inserta en el nodo hijo derecho. Si el nodo hijo no existe, lo crea.

4. La clase `NodoArbolBinario` tiene un método `altura` que devuelve la altura del árbol binario. La altura de un árbol binario es la longitud de la ruta más larga desde la raíz hasta una hoja.

5. La clase `NodoArbolBinario` tiene un método `imprimir` que imprime el árbol binario en orden. El método imprime primero el nodo hijo izquierdo, luego el nodo actual y luego el nodo hijo derecho.

6. En la función `main`, creamos un árbol binario con los datos `10`, `5`, `15`, `3`, `7`, `12` y `20`.

7. Imprimimos la altura del árbol binario y el árbol binario en orden.

El código anterior crea un árbol binario de búsqueda a partir de una lista de datos y luego imprime la altura del árbol y el árbol en orden.