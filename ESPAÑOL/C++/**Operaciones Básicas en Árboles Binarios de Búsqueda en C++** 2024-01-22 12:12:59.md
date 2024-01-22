```c++
// Un código complejo en C++ que crea un árbol binario de búsqueda y realiza varias operaciones sobre él.

#include <iostream>
using namespace std;

// Definición de la clase Nodo.
class Nodo {
public:
    int dato;
    Nodo* izquierda;
    Nodo* derecha;

    // Constructor del nodo.
    Nodo(int dato) {
        this->dato = dato;
        this->izquierda = NULL;
        this->derecha = NULL;
    }
};

// Definición de la clase Árbol Binario de Búsqueda.
class ArbolBinarioDeBusqueda {
public:
    Nodo* raíz;

    // Constructor del árbol.
    ArbolBinarioDeBusqueda() {
        this->raíz = NULL;
    }

    // Método para insertar un nodo en el árbol.
    void insertar(int dato) {
        Nodo* nuevoNodo = new Nodo(dato);

        if (raíz == NULL) {
            raíz = nuevoNodo;
        } else {
            insertarRecursivo(raíz, nuevoNodo);
        }
    }

    // Método para insertar un nodo en el árbol de forma recursiva.
    void insertarRecursivo(Nodo* nodoActual, Nodo* nuevoNodo) {
        if (nuevoNodo->dato < nodoActual->dato) {
            if (nodoActual->izquierda == NULL) {
                nodoActual->izquierda = nuevoNodo;
            } else {
                insertarRecursivo(nodoActual->izquierda, nuevoNodo);
            }
        } else {
            if (nodoActual->derecha == NULL) {
                nodoActual->derecha = nuevoNodo;
            } else {
                insertarRecursivo(nodoActual->derecha, nuevoNodo);
            }
        }
    }

    // Método para buscar un nodo en el árbol.
    bool buscar(int dato) {
        return buscarRecursivo(raíz, dato);
    }

    // Método para buscar un nodo en el árbol de forma recursiva.
    bool buscarRecursivo(Nodo* nodoActual, int dato) {
        if (nodoActual == NULL) {
            return false;
        } else if (nodoActual->dato == dato) {
            return true;
        } else if (dato < nodoActual->dato) {
            return buscarRecursivo(nodoActual->izquierda, dato);
        } else {
            return buscarRecursivo(nodoActual->derecha, dato);
        }
    }

    // Método para eliminar un nodo del árbol.
    void eliminar(int dato) {
        eliminarRecursivo(raíz, dato);
    }

    // Método para eliminar un nodo del árbol de forma recursiva.
    Nodo* eliminarRecursivo(Nodo* nodoActual, int dato) {
        if (nodoActual == NULL) {
            return nodoActual;
        } else if (dato < nodoActual->dato) {
            nodoActual->izquierda = eliminarRecursivo(nodoActual->izquierda, dato);
        } else if (dato > nodoActual->dato) {
            nodoActual->derecha = eliminarRecursivo(nodoActual->derecha, dato);
        } else {
            if (nodoActual->izquierda == NULL) {
                Nodo* nodoTemp = nodoActual->derecha;
                delete nodoActual;
                return nodoTemp;
            } else if (nodoActual->derecha == NULL) {
                Nodo* nodoTemp = nodoActual->izquierda;
                delete nodoActual;
                return nodoTemp;
            } else {
                Nodo* nodoTemp = nodoActual->derecha;
                while (nodoTemp->izquierda != NULL) {
                    nodoTemp = nodoTemp->izquierda;
                }
                nodoActual->dato = nodoTemp->dato;
                nodoActual->derecha = eliminarRecursivo(nodoActual->derecha, nodoTemp->dato);
            }
        }
        return nodoActual;
    }

    // Método para imprimir el árbol en orden ascendente.
    void imprimirEnOrden() {
        imprimirEnOrdenRecursivo(raíz);
    }

    // Método para imprimir el árbol en orden ascendente de forma recursiva.
    void imprimirEnOrdenRecursivo(Nodo* nodoActual) {
        if (nodoActual != NULL) {
            imprimirEnOrdenRecursivo(nodoActual->izquierda);
            cout << nodoActual->dato << " ";
            imprimirEnOrdenRecursivo(nodoActual->derecha);
        }
    }

    // Método para imprimir el árbol en preorden.
    void imprimirEnPreOrden() {
        imprimirEnPreOrdenRecursivo(raíz);
    }

    // Método para imprimir el árbol en preorden de forma recursiva.
    void imprimirEnPreOrdenRecursivo(Nodo* nodoActual) {
        if (nodoActual != NULL) {
            cout << nodoActual->dato << " ";
            imprimirEnPreOrdenRecursivo(nodoActual->izquierda);
            imprimirEnPreOrdenRecursivo(nodoActual->derecha);
        }
    }

    // Método para imprimir el árbol en postorden.
    void imprimirEnPostOrden() {
        imprimirEnPostOrdenRecursivo(raíz);
    }

    // Método para imprimir el árbol en postorden de forma recursiva.
    void imprimirEnPostOrdenRecursivo(Nodo* nodoActual) {
        if (nodoActual != NULL) {
            imprimirEnPostOrdenRecursivo(nodoActual->izquierda);
            imprimirEnPostOrdenRecursivo(nodoActual->derecha);
            cout << nodoActual->dato << " ";
        }
    }

    // Método para encontrar el nodo mínimo en el árbol.
    Nodo* encontrarNodoMínimo() {
        return encontrarNodoMínimoRecursivo(raíz);
    }

    // Método para encontrar el nodo mínimo en el árbol de forma recursiva.
    Nodo* encontrarNodoMínimoRecursivo(Nodo* nodoActual) {
        if (nodoActual == NULL) {
            return nodoActual;
        } else if (nodoActual->izquierda == NULL) {
            return nodoActual;
        } else {
            return encontrarNodoMínimoRecursivo(nodoActual->izquierda);
        }
    }

    // Método para encontrar el nodo máximo en el árbol.
    Nodo* encontrarNodoMáximo() {
        return encontrarNodoMáximoRecursivo(raíz);
    }

    // Método para encontrar el nodo máximo en el árbol de forma recursiva.
    Nodo* encontrarNodoMáximoRecursivo(Nodo* nodoActual) {
        if (nodoActual == NULL) {
            return nodoActual;
        } else if (nodoActual->derecha == NULL) {
            return nodoActual;
        } else {
            return encontrarNodoMáximoRecursivo(nodoActual->derecha);
        }
    }

    // Método para encontrar el nodo sucesor de un nodo dado.
    Nodo* encontrarSucesor(int dato) {
        return encontrarSucesorRecursivo(raíz, dato);
    }

    // Método para encontrar el nodo sucesor de un nodo dado de forma recursiva.
    Nodo* encontrarSucesorRecursivo(Nodo* nodoActual, int dato) {
        if (nodoActual == NULL) {
            return nodoActual;
        } else if (nodoActual->dato == dato) {
            if (nodoActual->derecha != NULL) {
                return encontrarNodoMínimoRecursivo(nodoActual->derecha);
            } else {
                Nodo* nodoPadre = nodoActual->padre;
                while (nodoPadre != NULL && nodoActual == nodoPadre->derecha) {
                    nodoActual = nodoPadre;
                    nodoPadre = nodoPadre->padre;
                }
                return nodoPadre;
            }
        } else if (dato < nodoActual->dato) {
            return encontrarSucesorRecursivo(nodoActual->izquierda, dato);
        } else {
            return encontrarSucesorRecursivo(nodoActual->derecha, dato);
        }
    }

    // Método para encontrar el nodo predecesor de un nodo dado.
    Nodo* encontrarPredecesor(int dato) {
        return encontrarPredecesorRecursivo(raíz, dato);
    }

    // Método para encontrar el nodo predecesor de un nodo dado de forma recursiva.
    Nodo* encontrarPredecesorRecursivo(Nodo* nodoActual, int dato) {
        if (nodoActual == NULL) {
            return nodoActual;
        } else if (nodoActual->dato == dato) {
            if (nodoActual->izquierda != NULL) {
                return encontrarNodoMáximoRecursivo(nodoActual->izquierda);
            } else {
                Nodo* nodoPadre = nodoActual->padre;
                while (nodoPadre != NULL && nodoActual == nodoPadre->izquierda) {
                    nodoActual = nodoPadre;
                    nodoPadre = nodoPadre->padre;
                }
                return nodoPadre;
            }
        } else if (dato < nodoActual->dato) {
            return encontrarPredecesorRecursivo(nodoActual->izquierda, dato);
        } else {
            return encontrarPredecesorRecursivo(nodoActual->derecha, dato);
        }
    }

    // Método para encontrar la altura del árbol.
    int encontrarAltura() {
        return encontrarAlturaRecursivo(raíz);
    }

    // Método para encontrar la altura del árbol de forma recursiva.
    int encontrarAlturaRecursivo(Nodo* nodoActual) {
        if (nodoActual == NULL) {
            return 0;
        } else {
            int alturaIzquierda = encontrarAlturaRecursivo(nodoActual->izquierda);
            int alturaDerecha = encontrarAlturaRecursivo(nodoActual->derecha);
            return 1 + max(alturaIzquierda, alturaDerecha);
        }
    }

    // Método para encontrar el nodo ancestro común más cercano de dos nodos dados.
    Nodo* encontrarAncestroComúnMásCercano(int dato1, int dato2) {
        return encontrarAncestroComúnMásCercanoRecursivo(raíz, dato1, dato2);
    }

    // Método para encontrar el nodo ancestro común más cercano de dos nodos dados de forma recursiva.
    Nodo* encontrarAncestroComúnMásCercanoRecursivo(Nodo* nodoActual, int dato1, int dato2) {
        if (nodoActual == NULL) {
            return nodoActual;
        } else if (nodoActual->dato == dato1 || nodoActual->dato == dato2) {
            return nodoActual;
        } else {
            Nodo* nodoIzquierdo = encontrarAncestroComúnMásCercanoRecursivo(nodoActual->izquierda, dato1, dato2);
            Nodo* nodoDerecho = encontrarAncestroComúnMásCercanoRecursivo(nodoActual->derecha, dato1, dato2);
            if (nodoIzquierdo != NULL && nodoDerecho != NULL) {
                return nodoActual;
            } else if (nodoIzquierdo != NULL) {
                return nodoIzquierdo;
            } else {
                return nodoDerecho;
            }
        }
    }
};

int main() {
    // Crear un árbol binario de búsqueda
    ÁrbolBinarioDeBúsqueda árbol;

    // Insertar nodos en el árbol
    árbol.insertar(10);
    árbol.insertar(5);
    árbol.insertar(15);
    árbol.insertar(2);
    árbol.insertar(7);
    árbol.insertar(12);
    árbol.insertar(20);

    // Imprimir el árbol en orden ascendente
    cout << "Imprimir el árbol en orden ascendente: ";
    árbol.imprimirEnOrden();
    cout << endl;

    // Imprimir el árbol en preorden
    cout << "Imprimir el árbol en preorden: ";
    árbol.imprimirEnPreOrden();
    cout << endl;

    // Imprimir el árbol en postorden
    cout << "Imprimir el árbol en postorden: ";
    árbol.imprimirEnPostOrden();
    cout << endl;

    // Encontrar el nodo mínimo en el árbol
    Nodo* nodoMínimo = árbol.encontrarNodoMínimo();
    cout << "El nodo mínimo en el árbol es: " << nodoMínimo->dato << endl;

    // Encontrar el nodo máximo en el árbol
    Nodo* nodoMáximo = árbol.encontrarNodoMáximo();
    cout << "El nodo máximo en el árbol es: " << nodoMáximo->dato << endl;

    // Encontrar el nodo sucesor de un nodo dado
    Nodo* nodoSucesor = árbol.encontrarSucesor(10);
    cout << "El nodo sucesor de 10 es: " << nodoSucesor->dato << endl;

    // Encontrar el nodo predecesor de un nodo dado
    Nodo* nodoPredecesor = árbol.encontrarPredecesor(10);
    cout << "El nodo predecesor de 10 es: " << nodoPredecesor->dato << endl;

    // Encontrar la altura del árbol
    int altura = árbol.encontrarAltura();
    cout << "La altura del árbol es: " << altura << endl;

    // Encontrar el nodo ancestro común más cercano de dos nodos dados
    Nodo* nodoAncestroComúnMásCercano = árbol.encontrarAncestroComúnMásCercano(2, 20);
    cout << "El nodo ancestro común más cercano de 2 y 20 es: "