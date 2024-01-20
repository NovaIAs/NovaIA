```c++
// Incluimos las librerías necesarias
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>

// Definimos la clase Nodo que será utilizada para construir el árbol binario de búsqueda
class Nodo {
public:
    int valor;
    Nodo* izquierdo;
    Nodo* derecho;

    // Constructor de la clase Nodo
    Nodo(int valor) {
        this->valor = valor;
        this->izquierdo = nullptr;
        this->derecho = nullptr;
    }
};

// Definimos la clase ÁrbolBinarioDeBúsqueda que será utilizada para manipular el árbol
class ÁrbolBinarioDeBúsqueda {
public:
    Nodo* raiz;

    // Constructor del árbol binario de búsqueda
    ÁrbolBinarioDeBúsqueda() {
        this->raiz = nullptr;
    }

    // Método para insertar un nuevo valor en el árbol
    void insertar(int valor) {
        // Si el árbol está vacío, creamos un nuevo nodo y lo asignamos como raíz
        if (this->raiz == nullptr) {
            this->raiz = new Nodo(valor);
            return;
        }

        // Si el árbol no está vacío, llamamos a la función privada insertarRecursivamente para insertar el nuevo valor
        insertarRecursivamente(this->raiz, valor);
    }

    // Método recursivo para insertar un nuevo valor en el árbol
    void insertarRecursivamente(Nodo* nodoActual, int valor) {
        // Si el valor es menor que el valor del nodo actual, vamos al nodo izquierdo
        if (valor < nodoActual->valor) {
            // Si el nodo izquierdo es nulo, creamos un nuevo nodo y lo asignamos como nodo izquierdo
            if (nodoActual->izquierdo == nullptr) {
                nodoActual->izquierdo = new Nodo(valor);
                return;
            }

            // Si el nodo izquierdo no es nulo, seguimos recursivamente por el nodo izquierdo
            else {
                insertarRecursivamente(nodoActual->izquierdo, valor);
            }
        }

        // Si el valor es mayor o igual que el valor del nodo actual, vamos al nodo derecho
        else {
            // Si el nodo derecho es nulo, creamos un nuevo nodo y lo asignamos como nodo derecho
            if (nodoActual->derecho == nullptr) {
                nodoActual->derecho = new Nodo(valor);
                return;
            }

            // Si el nodo derecho no es nulo, seguimos recursivamente por el nodo derecho
            else {
                insertarRecursivamente(nodoActual->derecho, valor);
            }
        }
    }

    // Método para buscar un valor en el árbol
    bool buscar(int valor) {
        // Si el árbol está vacío, devolvemos falso
        if (this->raiz == nullptr) {
            return false;
        }

        // Si el árbol no está vacío, llamamos a la función privada buscarRecursivamente para buscar el valor
        return buscarRecursivamente(this->raiz, valor);
    }

    // Método recursivo para buscar un valor en el árbol
    bool buscarRecursivamente(Nodo* nodoActual, int valor) {
        // Si el nodo actual es nulo, devolvemos falso
        if (nodoActual == nullptr) {
            return false;
        }

        // Si el valor del nodo actual es igual al valor que buscamos, devolvemos verdadero
        if (nodoActual->valor == valor) {
            return true;
        }

        // Si el valor del nodo actual es mayor que el valor que buscamos, vamos al nodo izquierdo
        if (nodoActual->valor > valor) {
            return buscarRecursivamente(nodoActual->izquierdo, valor);
        }

        // Si el valor del nodo actual es menor que el valor que buscamos, vamos al nodo derecho
        else {
            return buscarRecursivamente(nodoActual->derecho, valor);
        }
    }

    // Método para eliminar un valor del árbol
    void eliminar(int valor) {
        // Si el árbol está vacío, no hacemos nada
        if (this->raiz == nullptr) {
            return;
        }

        // Si el árbol no está vacío, llamamos a la función privada eliminarRecursivamente para eliminar el valor
        eliminarRecursivamente(this->raiz, valor);
    }

    // Método recursivo para eliminar un valor del árbol
    Nodo* eliminarRecursivamente(Nodo* nodoActual, int valor) {
        // Si el nodo actual es nulo, devolvemos nulo
        if (nodoActual == nullptr) {
            return nullptr;
        }

        // Si el valor del nodo actual es mayor que el valor que queremos eliminar, vamos al nodo izquierdo
        if (nodoActual->valor > valor) {
            nodoActual->izquierdo = eliminarRecursivamente(nodoActual->izquierdo, valor);
            return nodoActual;
        }

        // Si el valor del nodo actual es menor que el valor que queremos eliminar, vamos al nodo derecho
        if (nodoActual->valor < valor) {
            nodoActual->derecho = eliminarRecursivamente(nodoActual->derecho, valor);
            return nodoActual;
        }

        // Si el valor del nodo actual es igual al valor que queremos eliminar, tenemos tres casos posibles:

        // Caso 1: El nodo actual no tiene hijos
        if (nodoActual->izquierdo == nullptr && nodoActual->derecho == nullptr) {
            delete nodoActual;
            return nullptr;
        }

        // Caso 2: El nodo actual tiene un solo hijo, ya sea izquierdo o derecho
        if (nodoActual->izquierdo != nullptr && nodoActual->derecho == nullptr) {
            Nodo* nodoHijo = nodoActual->izquierdo;
            delete nodoActual;
            return nodoHijo;
        }

        if (nodoActual->izquierdo == nullptr && nodoActual->derecho != nullptr) {
            Nodo* nodoHijo = nodoActual->derecho;
            delete nodoActual;
            return nodoHijo;
        }

        // Caso 3: El nodo actual tiene dos hijos
        // Buscamos el nodo con el valor mínimo en el subárbol derecho
        Nodo* nodoMinimo = nodoActual->derecho;
        while (nodoMinimo->izquierdo != nullptr) {
            nodoMinimo = nodoMinimo->izquierdo;
        }

        // Copiamos el valor del nodo mínimo en el nodo actual
        nodoActual->valor = nodoMinimo->valor;

        // Eliminamos el nodo mínimo del subárbol derecho
        nodoActual->derecho = eliminarRecursivamente(nodoActual->derecho, nodoMinimo->valor);

        return nodoActual;
    }

    // Método para recorrer el árbol en preorden
    void recorrerPreorden() {
        recorrerPreordenRecursivamente(this->raiz);
    }

    // Método recursivo para recorrer el árbol en preorden
    void recorrerPreordenRecursivamente(Nodo* nodoActual) {
        // Si el nodo actual es nulo, no hacemos nada
        if (nodoActual == nullptr) {
            return;
        }

        // Visitamos el nodo actual
        std::cout << nodoActual->valor << " ";

        // Recorremos el subárbol izquierdo
        recorrerPreordenRecursivamente(nodoActual->izquierdo);

        // Recorremos el subárbol derecho
        recorrerPreordenRecursivamente(nodoActual->derecho);
    }

    // Método para recorrer el árbol en inorden
    void recorrerInorden() {
        recorrerInordenRecursivamente(this->raiz);
    }

    // Método recur