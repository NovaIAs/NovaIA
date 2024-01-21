```d
import std.stdio, std.string, std.array;
import std.algorithm;
import std.variant;
import std.set;

// Clase para representar un conjunto de objetos de un tipo genérico
class Conjunto<T>(T[] elementos) {
    private T[] _elementos = new T[elementos.length];

    // Constructor para inicializar el conjunto con los elementos especificados
    this(elementos) {
        _elementos = elementos;
    }

    // Método que comprueba si un elemento está en el conjunto
    in (T elemento) {
        return _elementos.indexOf(elemento) != -1;
    }

    // Método que añade un elemento al conjunto
    void add(T elemento) {
        if (!in(elemento)) {
            T[] nuevo = new T[_elementos.length + 1];
            for (int i = 0; i < _elementos.length; i++) {
                nuevo[i] = _elementos[i];
            }
            nuevo[_elementos.length] = elemento;
            _elementos = nuevo;
        }
    }

    // Método que elimina un elemento del conjunto
    void remove(T elemento) {
        if (in(elemento)) {
            int indice = _elementos.indexOf(elemento);
            T[] nuevo = new T[_elementos.length - 1];
            for (int i = 0; i < indice; i++) {
                nuevo[i] = _elementos[i];
            }
            for (int i = indice + 1; i < _elementos.length; i++) {
                nuevo[i - 1] = _elementos[i];
            }
            _elementos = nuevo;
        }
    }

    // Método que devuelve el tamaño del conjunto
    size_t size() {
        return _elementos.length;
    }

    // Método que devuelve una copia del conjunto
    Conjunto copy() {
        return new Conjunto(_elementos);
    }

    // Método que devuelve una cadena de caracteres con los elementos del conjunto separados por comas
    string toString() {
        string s = "(";
        for (int i = 0; i < _elementos.length; i++) {
            s += _elementos[i].toString();
            if (i < _elementos.length - 1) {
                s += ", ";
            }
        }
        s += ")";
        return s;
    }
}

// Clase para representar una relación entre dos conjuntos
class Relacion<A, B>(Conjunto<A> dominio, Conjunto<B> codominio, (A, B)[] pares) {
    private Conjunto<A> _dominio = dominio.copy();
    private Conjunto<B> _codominio = codominio.copy();
    private (A, B)[] _pares = new (A, B)[pares.length];

    // Constructor para inicializar la relación con los conjuntos y los pares especificados
    this(dominio, codominio, pares) {
        _dominio = dominio;
        _codominio = codominio;
        _pares = pares;
    }

    // Método que comprueba si un par pertenece a la relación
    in ((A, B) par) {
        return _pares.indexOf(par) != -1;
    }

    // Método que añade un par a la relación
    void add((A, B) par) {
        if (!in(par)) {
            (A, B)[] nuevo = new (A, B)[_pares.length + 1];
            for (int i = 0; i < _pares.length; i++) {
                nuevo[i] = _pares[i];
            }
            nuevo[_pares.length] = par;
            _pares = nuevo;
        }
    }

    // Método que elimina un par de la relación
    void remove((A, B) par) {
        if (in(par)) {
            int indice = _pares.indexOf(par);
            (A, B)[] nuevo = new (A, B)[_pares.length - 1];
            for (int i = 0; i < indice; i++) {
                nuevo[i] = _pares[i];