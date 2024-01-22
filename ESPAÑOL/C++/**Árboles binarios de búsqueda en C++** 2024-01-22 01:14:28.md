```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>

using namespace std;

// Clase Nodo para representar un nodo del árbol
class Nodo {
public:
    int valor;
    Nodo* izquierdo;
    Nodo* derecho;

    Nodo(int valor) {
        this->valor = valor;
        izquierdo = NULL;
        derecho = NULL;
    }
};

// Clase Árbol para representar un árbol binario de búsqueda
class Árbol {
public:
    Nodo* raíz;

    Árbol() {
        raíz = NULL;
    }

    // Función para insertar un nuevo nodo en el árbol
    void insertar(int valor) {
        Nodo* nuevoNodo = new Nodo(valor);
        insertarNodo(nuevoNodo, raíz);
    }

    // Función para insertar un nuevo nodo en el árbol de forma recursiva
    void insertarNodo(Nodo* nuevoNodo, Nodo* nodoActual) {
        if (nodoActual == NULL) {
            raíz = nuevoNodo;
        } else if (nuevoNodo->valor < nodoActual->valor) {
            if (nodoActual->izquierdo == NULL) {
                nodoActual->izquierdo = nuevoNodo;
            } else {
                insertarNodo(nuevoNodo, nodoActual->izquierdo);
            }
        } else {
            if (nodoActual->derecho == NULL) {
                nodoActual->derecho = nuevoNodo;
            } else {
                insertarNodo(nuevoNodo, nodoActual->derecho);
            }
        }
    }

    // Función para buscar un nodo en el árbol
    bool buscar(int valor) {
        return buscarNodo(valor, raíz);
    }

    // Función para buscar un nodo en el árbol de forma recursiva
    bool buscarNodo(int valor, Nodo* nodoActual) {
        if (nodoActual == NULL) {
            return false;
        } else if (nodoActual->valor == valor) {
            return true;
        } else if (valor < nodoActual->valor) {
            return buscarNodo(valor, nodoActual->izquierdo);
        } else {
            return buscarNodo(valor, nodoActual->derecho);
        }
    }

    // Función para eliminar un nodo del árbol
    void eliminar(int valor) {
        eliminarNodo(valor, raíz);
    }

    // Función para eliminar un nodo del árbol de forma recursiva
    void eliminarNodo(int valor, Nodo* nodoActual) {
        if (nodoActual == NULL) {
            return;
        } else if (valor < nodoActual->valor) {
            eliminarNodo(valor, nodoActual->izquierdo);
        } else if (valor > nodoActual->valor) {
            eliminarNodo(valor, nodoActual->derecho);
        } else {
            // Si el nodo tiene dos hijos
            if (nodoActual->izquierdo != NULL && nodoActual->derecho != NULL) {
                // Encontramos el nodo más a la izquierda en el subárbol derecho
                Nodo* nodoReemplazante = nodoActual->derecho;
                while (nodoReemplazante->izquierdo != NULL) {
                    nodoReemplazante = nodoReemplazante->izquierdo;
                }

                // Reemplazamos el valor del nodo actual con el valor del nodo reemplazante
                nodoActual->valor = nodoReemplazante->valor;

                // Eliminamos el nodo reemplazante
                eliminarNodo(nodoReemplazante->valor, nodoActual->derecho);
            } else {
                // Si el nodo tiene un solo hijo o ningún hijo
                Nodo* nodoHijo = nodoActual->izquierdo;
                if (nodoHijo == NULL) {
                    nodoHijo = nodoActual->derecho;
                }

                // Reemplazamos el nodo actual con su hijo
                nodoActual = nodoHijo;
            }
        }
    }

    // Función para imprimir el árbol en orden
    void imprimirEnOrden() {
        imprimirEnOrdenRecursivo(raíz);
        cout << endl;
    }

    // Función para imprimir el árbol en orden de forma recursiva
    void imprimirEnOrdenRecursivo(Nodo* nodoActual) {
        if (nodoActual == NULL) {
            return;
        }

        imprimirEnOrdenRecursivo(nodoActual->izquierdo);
        cout << nodoActual->valor << " ";
        imprimirEnOrdenRecursivo(nodoActual->derecho);
    }

    // Función para imprimir el árbol en preorden
    void imprimirEnPreOrden() {
        imprimirEnPreOrdenRecursivo(raíz);
        cout << endl;
    }

    // Función para imprimir el árbol en preorden de forma recursiva
    void imprimirEnPreOrdenRecursivo(Nodo* nodoActual) {
        if (nodoActual == NULL) {
            return;
        }

        cout << nodoActual->valor << " ";
        imprimirEnPreOrdenRecursivo(nodoActual->izquierdo);
        imprimirEnPreOrdenRecursivo(nodoActual->derecho);
    }

    // Función para imprimir el árbol en postorden
    void imprimirEnPostOrden() {
        imprimirEnPostOrdenRecursivo(raíz);
        cout << endl;
    }

    // Función para imprimir el árbol en postorden de forma recursiva
    void imprimirEnPostOrdenRecursivo(Nodo* nodoActual) {
        if (nodoActual == NULL) {
            return;
        }

        imprimirEnPostOrdenRecursivo(nodoActual->izquierdo);
        imprimirEnPostOrdenRecursivo(nodoActual->derecho);
        cout << nodoActual->valor << " ";
    }
};

int main() {
    // Creamos un árbol binario de búsqueda
    Árbol arbol;

    // Insertamos algunos valores en el árbol
    arbol.insertar(10);
    arbol.insertar(5);
    arbol.insertar(15);
    arbol.insertar(2);
    arbol.insertar(7);
    arbol.insertar(12);
    arbol.insertar(20);

    // Imprimimos el árbol en orden
    cout << "Árbol en orden: ";
    arbol.imprimirEnOrden();

    // Imprimimos el árbol en preorden
    cout << "Árbol en preorden: ";
    arbol.imprimirEnPreOrden();

    // Imprimimos el árbol en postorden
    cout << "Árbol en postorden: ";
    arbol.imprimirEnPostOrden();

    // Buscamos un valor en el árbol
    cout << "Valor 7 encontrado: " << (arbol.buscar(7) ? "Sí" : "No") << endl;

    // Eliminamos un valor del árbol
    arbol.eliminar(15);

    // Imprimimos el árbol en orden
    cout << "Árbol en orden después de eliminar el valor 15: ";
    arbol.imprimirEnOrden();

    return 0;
}
```

Este código implementa un árbol binario de búsqueda en C++. Un árbol binario de búsqueda es una estructura de datos que almacena valores de forma ordenada. Los valores se insertan en el árbol de tal manera que los valores más pequeños se encuentran en el lado izquierdo del árbol, y los valores más grandes se encuentran en el lado derecho del árbol. Esto permite realizar búsquedas, inserciones y eliminaciones de valores de forma muy eficiente.

El código define una clase Nodo que representa un nodo del árbol. Cada nodo tiene un valor, un puntero al nodo izquierdo y un puntero al nodo derecho.

El código también define una clase Árbol que representa un árbol binario de búsqueda. La clase Árbol contiene un puntero al nodo raíz del árbol.

El código incluye las siguientes funciones:

* insertar: esta función inserta un nuevo valor en el árbol.
* buscar: esta función busca un valor en el árbol.
* eliminar: esta función elimina un valor del árbol.
* imprimirEnOrden: esta función imprime el árbol en orden.
* imprimirEnPreOrden: esta función imprime el árbol en preorden.
* imprimirEnPostOrden: esta función imprime el árbol en postorden.

El código principal crea un árbol binario de búsqueda e inserta algunos valores en el árbol. Luego, imprime el árbol en orden, preorden y postorden. Luego, busca un valor en el árbol y lo elimina. Finalmente, imprime el árbol en orden nuevamente.