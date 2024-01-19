```c++
// Incluimos las bibliotecas necesarias
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <algorithm>

// Definimos el tipo de dato para los nodos del árbol
struct Nodo {
    int valor;
    Nodo* izq;
    Nodo* der;
};

// Creamos una función para crear un nuevo nodo
Nodo* nuevoNodo(int valor) {
    Nodo* nodo = new Nodo;
    nodo->valor = valor;
    nodo->izq = nullptr;
    nodo->der = nullptr;
    return nodo;
}

// Creamos una función para insertar un nuevo nodo en el árbol
void insertar(Nodo*& raiz, int valor) {
    if (raiz == nullptr) {
        raiz = nuevoNodo(valor);
    } else if (valor < raiz->valor) {
        insertar(raiz->izq, valor);
    } else {
        insertar(raiz->der, valor);
    }
}

// Creamos una función para buscar un nodo en el árbol
Nodo* buscar(Nodo* raiz, int valor) {
    if (raiz == nullptr) {
        return nullptr;
    } else if (raiz->valor == valor) {
        return raiz;
    } else if (valor < raiz->valor) {
        return buscar(raiz->izq, valor);
    } else {
        return buscar(raiz->der, valor);
    }
}

// Creamos una función para eliminar un nodo del árbol
void eliminar(Nodo*& raiz, int valor) {
    if (raiz == nullptr) {
        return;
    } else if (raiz->valor == valor) {
        if (raiz->izq == nullptr && raiz->der == nullptr) {
            delete raiz;
            raiz = nullptr;
        } else if (raiz->izq == nullptr) {
            Nodo* aux = raiz;
            raiz = raiz->der;
            delete aux;
        } else if (raiz->der == nullptr) {
            Nodo* aux = raiz;
            raiz = raiz->izq;
            delete aux;
        } else {
            Nodo* aux = raiz->izq;
            while (aux->der != nullptr) {
                aux = aux->der;
            }
            raiz->valor = aux->valor;
            eliminar(raiz->izq, aux->valor);
        }
    } else if (valor < raiz->valor) {
        eliminar(raiz->izq, valor);
    } else {
        eliminar(raiz->der, valor);
    }
}

// Creamos una función para recorrer el árbol en preorden
void preorden(Nodo* raiz) {
    if (raiz != nullptr) {
        std::cout << raiz->valor << " ";
        preorden(raiz->izq);
        preorden(raiz->der);
    }
}

// Creamos una función para recorrer el árbol en inorden
void inorden(Nodo* raiz) {
    if (raiz != nullptr) {
        inorden(raiz->izq);
        std::cout << raiz->valor << " ";
        inorden(raiz->der);
    }
}

// Creamos una función para recorrer el árbol en postorden
void postorden(Nodo* raiz) {
    if (raiz != nullptr) {
        postorden(raiz->izq);
        postorden(raiz->der);
        std::cout << raiz->valor << " ";
    }
}

// Creamos una función para encontrar el nodo con el valor mínimo en el árbol
Nodo* minimo(Nodo* raiz) {
    if (raiz == nullptr) {
        return nullptr;
    } else if (raiz->izq == nullptr) {
        return raiz;
    } else {
        return minimo(raiz->izq);
    }
}

// Creamos una función para encontrar el nodo con el valor máximo en el árbol
Nodo* maximo(Nodo* raiz) {
    if (raiz == nullptr) {
        return nullptr;
    } else if