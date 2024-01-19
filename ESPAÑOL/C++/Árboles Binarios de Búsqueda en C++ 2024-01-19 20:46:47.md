```c++
// Implementación de un árbol binario de búsqueda (ABB) en C++.

#include <iostream>
#include <vector>

using namespace std;

// Clase Nodo que representa un nodo del ABB.
class Nodo {
public:
    int dato;
    Nodo* izquierdo;
    Nodo* derecho;

    // Constructor del nodo.
    Nodo(int dato) {
        this->dato = dato;
        this->izquierdo = nullptr;
        this->derecho = nullptr;
    }
};

// Clase ABB que representa el árbol binario de búsqueda.
class ABB {
private:
    Nodo* raiz;

    // Método recursivo para insertar un elemento en el ABB.
    void insertarRecursivo(Nodo* nodo, int dato) {
        if (dato < nodo->dato){
            if (nodo->izquierdo == nullptr) {
                nodo->izquierdo = new Nodo(dato);
            } else {
                insertarRecursivo(nodo->izquierdo, dato);
            }
        }else{
            if (nodo->derecho == nullptr) {
                nodo->derecho = new Nodo(dato);
            } else {
                insertarRecursivo(nodo->derecho, dato);
            }
        }
    }

    // Método iterativo para buscar un elemento en el ABB.
    Nodo* buscarIterativo(int dato) {
        Nodo* nodoActual = raiz;
        while (nodoActual != nullptr) {
            if (dato == nodoActual->dato) {
                return nodoActual;
            } else if (dato < nodoActual->dato) {
                nodoActual = nodoActual->izquierdo;
            } else {
                nodoActual = nodoActual->derecho;
            }
        }
        return nullptr;
    }

    // Método recursivo para eliminar un elemento del ABB.
    void eliminarRecursivo(Nodo* nodo, int dato) {
        if (nodo == nullptr) {
            return;
        }

        if (dato < nodo->dato) {
            eliminarRecursivo(nodo->izquierdo, dato);
        } else if (dato > nodo->dato) {
            eliminarRecursivo(nodo->derecho, dato);
        } else {
            // Caso 1: Nodo hoja
            if (nodo->izquierdo == nullptr && nodo->derecho == nullptr) {
                delete nodo;
                nodo = nullptr;
            }
            // Caso 2: Nodo con un solo hijo
            else if (nodo->izquierdo == nullptr) {
                Nodo* nodoAux = nodo;
                nodo = nodo->derecho;
                delete nodoAux;
            } else if (nodo->derecho == nullptr) {
                Nodo* nodoAux = nodo;
                nodo = nodo->izquierdo;
                delete nodoAux;
            }
            // Caso 3: Nodo con dos hijos
            else {
                // Buscamos el valor mínimo del subárbol derecho
                int minValor = nodo->derecho->dato;
                while (nodo->derecho->izquierdo != nullptr) {
                    minValor = nodo->derecho->izquierdo->dato;
                }

                // Sustituimos el valor del nodo a eliminar por el valor mínimo del subárbol derecho
                nodo->dato = minValor;

                // Eliminamos el nodo con el valor mínimo del subárbol derecho
                eliminarRecursivo(nodo->derecho, minValor);
            }
        }
    }

    // Método para imprimir el ABB en preorden.
    void imprimirPreorden(Nodo* nodo) {
        if (nodo == nullptr) {
            return;
        }

        cout << nodo->dato << " ";
        imprimirPreorden(nodo->izquierdo);
        imprimirPreorden(nodo->derecho);
    }

    // Método para imprimir el ABB en inorden.
    void imprimirInorden(Nodo* nodo) {
        if (nodo == nullptr) {
            return;
        }

        imprimirInorden(nodo->izquierdo);
        cout << nodo->dato << " ";
        imprimirInorden(nodo->derecho);
    }

    // Método para imprimir el ABB en postorden.
    void imprimirPostorden(Nodo* nodo) {
        if (nodo == nullptr) {
            return;
        }

        imprimirPostorden(nodo->izquierdo);
        imprimirPostorden(nodo->derecho);
        cout << nodo->dato << " ";
    }

public:
    // Constructor del ABB.
    ABB() {
        raiz = nullptr;
    }

    // Método para insertar un elemento en el ABB.
    void insertar(int dato) {
        if (raiz == nullptr) {
            raiz = new Nodo(dato);
        } else {
            insertarRecursivo(raiz, dato);
        }
    }

    // Método para buscar un elemento en el ABB.
    Nodo* buscar(int dato) {
        return buscarIterativo(dato);
    }

    // Método para eliminar un elemento del ABB.
    void eliminar(int dato) {
        eliminarRecursivo(raiz, dato);
    }

    // Método para imprimir el ABB en preorden.
    void imprimirPreorden() {
        imprimirPreorden(raiz);
        cout << endl;
    }

    // Método para imprimir el ABB en inorden.
    void imprimirInorden() {
        imprimirInorden(raiz);
        cout << endl;
    }

    // Método para imprimir el ABB en postorden.
    void imprimirPostorden() {
        imprimirPostorden(raiz);
        cout << endl;
    }
};

// Función principal.
int main() {
    ABB arbol;

    // Insertar algunos elementos en el ABB
    arbol.insertar(50);
    arbol.insertar(30);
    arbol.insertar(20);
    arbol.insertar(40);
    arbol.insertar(70);
    arbol.insertar(60);
    arbol.insertar(80);

    // Buscar un elemento en el ABB
    int datoABuscar = 40;
    Nodo* nodoEncontrado = arbol.buscar(datoABuscar);
    if (nodoEncontrado != nullptr) {
        cout << "El elemento " << datoABuscar << " se encuentra en el árbol." << endl;
    } else {
        cout << "El elemento " << datoABuscar << " no se encuentra en el árbol." << endl;
    }

    // Imprimir el ABB en preorden
    cout << "Impresión preorden: ";
    arbol.imprimirPreorden();

    // Imprimir el ABB en inorden
    cout << "Impresión inorden: ";
    arbol.imprimirInorden();

    // Imprimir el ABB en postorden
    cout << "Impresión postorden: ";
    arbol.imprimir