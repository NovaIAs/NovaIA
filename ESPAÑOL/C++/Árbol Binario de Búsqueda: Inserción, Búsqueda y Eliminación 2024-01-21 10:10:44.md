```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <string>

using namespace std;

// Estructura para representar un nodo en un árbol binario de búsqueda
struct Nodo {
    int valor;
    Nodo* izquierdo;
    Nodo* derecho;

    Nodo(int valor) {
        this->valor = valor;
        izquierdo = nullptr;
        derecho = nullptr;
    }
};

// Clase para representar un árbol binario de búsqueda
class ArbolBinarioDeBusqueda {
public:
    Nodo* raiz;

    ArbolBinarioDeBusqueda() {
        raiz = nullptr;
    }

    // Función para insertar un nuevo nodo en el árbol
    void insertar(int valor) {
        Nodo* nuevo_nodo = new Nodo(valor);
        if (raiz == nullptr) {
            raiz = nuevo_nodo;
        } else {
            insertar_recursivo(raiz, nuevo_nodo);
        }
    }

    // Función recursiva para insertar un nuevo nodo en el árbol
    void insertar_recursivo(Nodo* actual, Nodo* nuevo_nodo) {
        if (nuevo_nodo->valor < actual->valor) {
            if (actual->izquierdo == nullptr) {
                actual->izquierdo = nuevo_nodo;
            } else {
                insertar_recursivo(actual->izquierdo, nuevo_nodo);
            }
        } else {
            if (actual->derecho == nullptr) {
                actual->derecho = nuevo_nodo;
            } else {
                insertar_recursivo(actual->derecho, nuevo_nodo);
            }
        }
    }

    // Función para buscar un nodo en el árbol
    bool buscar(int valor) {
        return buscar_recursivo(raiz, valor);
    }

    // Función recursiva para buscar un nodo en el árbol
    bool buscar_recursivo(Nodo* actual, int valor) {
        if (actual == nullptr) {
            return false;
        } else if (actual->valor == valor) {
            return true;
        } else if (valor < actual->valor) {
            return buscar_recursivo(actual->izquierdo, valor);
        } else {
            return buscar_recursivo(actual->derecho, valor);
        }
    }

    // Función para eliminar un nodo del árbol
    void eliminar(int valor) {
        eliminar_recursivo(raiz, valor);
    }

    // Función recursiva para eliminar un nodo del árbol
    Nodo* eliminar_recursivo(Nodo* actual, int valor) {
        if (actual == nullptr) {
            return nullptr;
        } else if (valor < actual->valor) {
            actual->izquierdo = eliminar_recursivo(actual->izquierdo, valor);
        } else if (valor > actual->valor) {
            actual->derecho = eliminar_recursivo(actual->derecho, valor);
        } else {
            if (actual->izquierdo == nullptr) {
                Nodo* temp = actual->derecho;
                delete actual;
                return temp;
            } else if (actual->derecho == nullptr) {
                Nodo* temp = actual->izquierdo;
                delete actual;
                return temp;
            } else {
                Nodo* sucesor = encontrar_sucesor(actual->derecho);
                actual->valor = sucesor->valor;
                actual->derecho = eliminar_recursivo(actual->derecho, sucesor->valor);
            }
        }
        return actual;
    }

    // Función para encontrar el sucesor de un nodo en el árbol
    Nodo* encontrar_sucesor(Nodo* actual) {
        while (actual->izquierdo != nullptr) {
            actual = actual->izquierdo;
        }
        return actual;
    }

    // Función para imprimir el árbol en orden
    void imprimir_orden() {
        imprimir_orden_recursivo(raiz);
    }

    // Función recursiva para imprimir el árbol en orden
    void imprimir_orden_recursivo(Nodo* actual) {
        if (actual == nullptr) {
            return;
        }

        imprimir_orden_recursivo(actual->izquierdo);
        cout << actual->valor << " ";
        imprimir_orden_recursivo(actual->derecho);
    }

    // Función para imprimir el árbol en preorden
    void imprimir_preorden() {
        imprimir_preorden_recursivo(raiz);
    }

    // Función recursiva para imprimir el árbol en preorden
    void imprimir_preorden_recursivo(Nodo* actual) {
        if (actual == nullptr) {
            return;
        }

        cout << actual->valor << " ";
        imprimir_preorden_recursivo(actual->izquierdo);
        imprimir_preorden_recursivo(actual->derecho);
    }

    // Función para imprimir el árbol en postorden
    void imprimir_postorden() {
        imprimir_postorden_recursivo(raiz);