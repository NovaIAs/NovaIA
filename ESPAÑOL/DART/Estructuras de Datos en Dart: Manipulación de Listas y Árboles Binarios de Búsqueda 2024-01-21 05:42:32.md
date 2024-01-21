```
// Librería que contiene utilidades para manipular cadenas de texto.
import "dart:string_manipulations";

// Una clase que representa una lista de elementos.
class Lista {
    // Atributo que contiene los elementos de la lista.
    List<Object> elementos;

    // Constructor que inicializa la lista con una lista de elementos.
    Lista(List<Object> elementos) {
        this.elementos = elementos;
    }

    // Método que devuelve el elemento en la posición especificada.
    Object obtenerElemento(int posicion) {
        return this.elementos[posicion];
    }

    // Método que añade un elemento al final de la lista.
    void añadirElemento(Object elemento) {
        this.elementos.add(elemento);
    }

    // Método que elimina el elemento en la posición especificada.
    void eliminarElemento(int posicion) {
        this.elementos.removeAt(posicion);
    }

    // Método que devuelve el número de elementos en la lista.
    int obtenerTamaño() {
        return this.elementos.length;
    }

    // Método que devuelve una cadena de texto con los elementos de la lista separados por comas.
    String obtenerElementosComoCadena() {
        return this.elementos.join(",");
    }
}

// Una clase que representa un árbol binario de búsqueda.
class ArbolBinarioDeBusqueda {
    // Atributo que contiene el nodo raíz del árbol.
    Nodo raíz;

    // Constructor que inicializa el árbol con un nodo raíz.
    ArbolBinarioDeBusqueda(Nodo raíz) {
        this.raíz = raíz;
    }

    // Método que devuelve el nodo raíz del árbol.
    Nodo obtenerRaíz() {
        return this.raíz;
    }

    // Método que inserta un elemento en el árbol.
    void insertarElemento(Object elemento) {
        this.raíz.insertarElemento(elemento);
    }

    // Método que elimina un elemento del árbol.
    void eliminarElemento(Object elemento) {
        this.raíz.eliminarElemento(elemento);
    }

    // Método que busca un elemento en el árbol.
    Object buscarElemento(Object elemento) {
        return this.raíz.buscarElemento(elemento);
    }

    // Método que devuelve una cadena de texto con los elementos del árbol en orden.
    String obtenerElementosEnOrden() {
        return this.raíz.obtenerElementosEnOrden();
    }

    // Método que devuelve una cadena de texto con los elementos del árbol en preorden.
    String obtenerElementosEnPreorden() {
        return this.raíz.obtenerElementosEnPreorden();
    }

    // Método que devuelve una cadena de texto con los elementos del árbol en postorden.
    String obtenerElementosEnPostorden() {