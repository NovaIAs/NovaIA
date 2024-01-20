```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <string>

using namespace std;

// Funciones auxiliares
bool es_primo(int n) {
    if (n <= 1) {
        return false;
    }
    for (int i = 2; i * i <= n; i++) {
        if (n % i == 0) {
            return false;
        }
    }
    return true;
}

vector<int> factores_primos(int n) {
    vector<int> factores;
    for (int i = 2; i * i <= n; i++) {
        while (n % i == 0) {
            factores.push_back(i);
            n /= i;
        }
    }
    if (n > 1) {
        factores.push_back(n);
    }
    return factores;
}

// Clase nodo de un árbol binario
struct NodoArbol {
    int dato;
    NodoArbol *izquierda, *derecha;
    NodoArbol(int dato) : dato(dato), izquierda(nullptr), derecha(nullptr) {}
};

// Clase árbol binario
class ArbolBinario {
public:
    ArbolBinario() : raiz(nullptr) {}

    void insertar(int dato) {
        raiz = insertar_recursivo(raiz, dato);
    }

    void eliminar(int dato) {
        raiz = eliminar_recursivo(raiz, dato);
    }

    bool buscar(int dato) {
        return buscar_recursivo(raiz, dato);
    }

    void imprimir_preorden() {
        imprimir_preorden_recursivo(raiz);
    }

    void imprimir_inorden() {
        imprimir_inorden_recursivo(raiz);
    }

    void imprimir_postorden() {
        imprimir_postorden_recursivo(raiz);
    }

    int altura() {
        return altura_recursiva(raiz);
    }

    int tamaño() {
        return tamaño_recursivo(raiz);
    }

    int suma() {
        return suma_recursiva(raiz);
    }

private:
    NodoArbol *raiz;

    NodoArbol *insertar_recursivo(NodoArbol *nodo, int dato) {
        if (nodo == nullptr) {
            return new NodoArbol(dato);
        }
        if (dato < nodo->dato) {
            nodo->izquierda = insertar_recursivo(nodo->izquierda, dato);
        } else {
            nodo->derecha = insertar_recursivo(nodo->derecha, dato);
        }
        return nodo;
    }

    NodoArbol *eliminar_recursivo(NodoArbol *nodo, int dato) {
        if (nodo == nullptr) {
            return nullptr;
        }
        if (dato < nodo->dato) {
            nodo->izquierda = eliminar_recursivo(nodo->izquierda, dato);
        } else if (dato > nodo->dato) {
            nodo->derecha = eliminar_recursivo(nodo->derecha, dato);
        } else {
            if (nodo->izquierda == nullptr) {
                NodoArbol *temp = nodo->derecha;
                delete nodo;
                return temp;
            } else if (nodo->derecha == nullptr) {
                NodoArbol *temp = nodo->izquierda;
                delete nodo;
                return temp;
            } else {
                NodoArbol *temp = nodo->derecha;
                while (temp->izquierda != nullptr) {
                    temp = temp->izquierda;
                }
                nodo->dato = temp->dato;
                nodo->derecha