#include <iostream>
#include <vector>
#include <algorithm>

// Definir una clase personalizada para representar un nodo en un árbol binario
class Nodo {
public:
    int valor;
    Nodo* izquierda;
    Nodo* derecha;

    Nodo(int valor) {
        this->valor = valor;
        this->izquierda = nullptr;
        this->derecha = nullptr;
    }
};

// Definir una función para insertar un nuevo nodo en un árbol binario
void insertar(Nodo*& raiz, int valor) {
    // Si el árbol está vacío, crear un nuevo nodo como raíz
    if (raiz == nullptr) {
        raiz = new Nodo(valor);
        return;
    }

    // Si el valor es menor que el valor del nodo actual, insertar en el subárbol izquierdo
    if (valor < raiz->valor) {
        insertar(raiz->izquierda, valor);
    }
    // Si el valor es mayor o igual que el valor del nodo actual, insertar en el subárbol derecho
    else {
        insertar(raiz->derecha, valor);
    }
}

// Definir una función para buscar un nodo en un árbol binario
bool buscar(Nodo* raiz, int valor) {
    // Si el árbol está vacío, el nodo no existe
    if (raiz == nullptr) {
        return false;
    }

    // Si el valor es igual al valor del nodo actual, el nodo existe
    if (valor == raiz->valor) {
        return true;
    }

    // Si el valor es menor que el valor del nodo actual, buscar en el subárbol izquierdo
    if (valor < raiz->valor) {
        return buscar(raiz->izquierda, valor);
    }
    // Si el valor es mayor que el valor del nodo actual, buscar en el subárbol derecho
    else {
        return buscar(raiz->derecha, valor);
    }
}

// Definir una función para eliminar un nodo de un árbol binario
void eliminar(Nodo*& raiz, int valor) {
    // Si el árbol está vacío, el nodo no existe
    if (raiz == nullptr) {
        return;
    }

    // Si el valor es igual al valor del nodo actual, eliminar ese nodo
    if (valor == raiz->valor) {
        // Si el nodo no tiene hijos, simplemente eliminarlo
        if (raiz->izquierda == nullptr && raiz->derecha == nullptr) {
            delete raiz;
            raiz = nullptr;
            return;
        }

        // Si el nodo tiene un solo hijo, reemplazarlo con el hijo
        if (raiz->izquierda == nullptr) {
            Nodo* temp = raiz;
            raiz = raiz->derecha;
            delete temp;
            return;
        } else if (raiz->derecha == nullptr) {
            Nodo* temp = raiz;
            raiz = raiz->izquierda;
            delete temp;
            return;
        }

        // Si el nodo tiene dos hijos, encontrar el nodo más a la izquierda en el subárbol derecho y reemplazarlo con el nodo actual
        Nodo* temp = raiz->derecha;
        while (temp->izquierda != nullptr) {
            temp = temp->izquierda;
        }

        raiz->valor = temp->valor;
        eliminar(raiz->derecha, temp->valor);
        return;
    }

    // Si el valor es menor que el valor del nodo actual, buscar en el subárbol izquierdo
    if (valor < raiz->valor) {
        eliminar(raiz->izquierda, valor);
    }
    // Si el valor es mayor que el valor del nodo actual, buscar en el subárbol derecho
    else {
        eliminar(raiz->derecha, valor);
    }
}

// Definir una función para recorrer el árbol binario en orden preorden
void recorrerPreorden(Nodo* raiz) {
    // Visitar el nodo actual
    std::cout << raiz->valor << " ";

    // Recorrer el subárbol izquierdo
    if (raiz->izquierda != nullptr) {
        recorrerPreorden(raiz->izquierda);
    }

    // Recorrer el subárbol derecho
    if (raiz->derecha != nullptr) {
        recorrerPreorden(raiz->derecha);
    }
}

// Definir una función para recorrer el árbol binario en orden inorden
void recorrerInorden(Nodo* raiz) {
    // Recorrer el subárbol izquierdo
    if (raiz->izquierda != nullptr) {
        recorrerInorden(raiz->izquierda);
    }

    // Visitar el nodo actual
    std::cout << raiz->valor << " ";

    // Recorrer el subárbol derecho
    if (raiz->derecha != nullptr) {
        recorrerInorden(raiz->derecha);
    }
}

// Definir una función para recorrer el árbol binario en orden posorden
void recorrerPosorden(Nodo* raiz) {
    // Recorrer el subárbol izquierdo
    if (raiz->izquierda != nullptr) {
        recorrerPosorden(raiz->izquierda);
    }

    // Recorrer el subárbol derecho
    if (raiz->derecha != nullptr) {
        recorrerPosorden(raiz->derecha);
    }

    // Visitar el nodo actual
    std::cout << raiz->valor << " ";
}

// Definir una función para determinar si un árbol binario es un árbol binario de búsqueda
bool esABB(Nodo* raiz) {
    // Si el árbol está vacío, es un ABB
    if (raiz == nullptr) {
        return true;
    }

    // Comprobar si el subárbol izquierdo es un ABB
    bool izquierdaABB = esABB(raiz->izquierda);

    // Comprobar si el subárbol derecho es un ABB
    bool derechaABB = esABB(raiz->derecha);

    // Comprobar si el valor del nodo actual es mayor que el valor máximo del subárbol izquierdo y menor que el valor mínimo del subárbol derecho
    int maxIzquierda = std::numeric_limits<int>::min();
    int minDerecha = std::numeric_limits<int>::max();
    if (raiz->izquierda != nullptr) {
        maxIzquierda = encontrarMaximo(raiz->izquierda);
    }
    if (raiz->derecha != nullptr) {
        minDerecha = encontrarMinimo(raiz->derecha);
    }
    bool valoresCorrectos = (raiz->valor > maxIzquierda && raiz->valor < minDerecha);

    // Si todas las condiciones se cumplen, el árbol es un ABB
    return izquierdaABB && derechaABB && valoresCorrectos;
}

// Función auxiliar para encontrar el valor máximo en un árbol binario
int encontrarMaximo(Nodo* raiz) {
    if (raiz == nullptr) {
        return std::numeric_limits<int>::min();
    }

    int max = raiz->valor;
    int izquierdaMax = encontrarMaximo(raiz->izquierda);
    int derechaMax = encontrarMaximo(raiz->derecha);

    if (izquierdaMax > max) {
        max = izquierdaMax;
    }
    if (derechaMax > max) {
        max = derechaMax;
    }

    return max;
}

// Función auxiliar para encontrar el valor mínimo en un árbol binario
int encontrarMinimo(Nodo* raiz) {
    if (raiz == nullptr) {
        return std::numeric_limits<int>::max();
    }

    int min = raiz->valor;
    int izquierdaMin = encontrarMinimo(raiz->izquierda);
    int derechaMin = encontrarMinimo(raiz->derecha);

    if (izquierdaMin < min) {
        min = izquierdaMin;
    }
    if (derechaMin < min) {
        min = derechaMin;
    }

    return min;
}

int main() {
    // Crear un árbol binario de búsqueda
    Nodo* raiz = nullptr;
    insertar(raiz, 10);
    insertar(raiz, 5);
    insertar(raiz, 15);
    insertar(raiz, 2);
    insertar(raiz, 7);
    insertar(raiz, 12);
    insertar(raiz, 20);

    // Insertar un nodo duplicado para probar la función de búsqueda
    insertar(raiz, 10);

    // Buscar un nodo en el árbol binario
    int valor = 15;
    bool encontrado = buscar(raiz, valor);

    // Imprimir si el nodo fue encontrado o no
    if (encontrado) {
        std::cout << "El nodo con valor " << valor << " fue encontrado en el árbol binario." << std::endl;
    } else {
        std::cout << "El nodo con valor " << valor << " no fue encontrado en el árbol binario." << std::endl;
    }

    // Recorrer el árbol binario en orden preorden
    std::cout << "Recorrido preorden: ";
    recorrerPreorden(raiz);
    std::cout << std::endl;

    // Recorrer el árbol binario en orden inorden
    std::cout << "Recorrido inorden: ";
    recorrerInorden(raiz);
    std::cout << std::endl;

    // Recorrer el árbol binario en orden posorden
    std::cout << "Recorrido posorden: ";
    recorrerPosorden(raiz);
    std::cout << std::endl;

    // Comprobar si el árbol binario es un árbol binario de búsqueda
    bool esABB = esABB(raiz);

    // Imprimir si el árbol es un árbol binario de búsqueda o no
    if (esABB) {
        std::cout << "El árbol binario es un árbol binario de búsqueda." << std::endl;
    } else {
        std::cout << "El árbol binario no es un árbol binario de búsqueda." << std::endl;
    }

    // Eliminar un nodo del árbol binario
    valor = 12;
    eliminar(raiz, valor);

    // Recorrer el árbol binario en orden inorden para verificar que el nodo fue eliminado
    std::cout << "Recorrido inorden después de eliminar el nodo con valor " << valor << ": ";
    recorrerInorden(raiz);
    std::cout << std::endl;

    // Destruir el árbol binario
    destruirArbol(raiz);

    return 0;
}