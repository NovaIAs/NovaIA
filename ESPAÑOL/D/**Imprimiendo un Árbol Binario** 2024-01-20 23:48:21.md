```d
import std.stdio, std.algorithm, std.range, std.string, std.typecons;

struct Bintree(immutable) {
    auto left, right;
    T value;

    this(T value) pure nothrow {
        this.value = value;
    }

    this(Bintree!L(T value)) pure nothrow {
        this.value = value;
        this.left = left.dup;
        this.right = right.dup;
    }

    static this constructFrom(Bintree!L left, Bintree!R right, T value) pure nothrow {
        Bintree ret(value);
        ret.left = left.dup;
        ret.right = right.dup;
        return ret;
    }

    immutable Bintree!L@left() pure nothrow {
        return left.dup;
    }

    immutable Bintree!R@right() pure nothrow {
        return right.dup;
    }

    T@value() pure nothrow {
        return value;
    }
}

alias Bintree!L = Bintree!(immutable);
alias Bintree!R = Bintree!(immutable);

Bintree!L crearArbolDePrueba() pure nothrow {
    return constructFrom(
        constructFrom(
            nullptr,
            nullptr,
            300
        ),
        constructFrom(
            nullptr,
            nullptr,
            400
        ),
        200
    );
}

void mostrarArbol(Bintree!L raiz) pure nothrow {
    writeln(raiz.value());
    si(raiz.left != nullptr) {
        mostrarArbol(raiz.left);
    }
    si(raiz.right != nullptr) {
        mostrarArbol(raiz.right);
    }
}

void main() pure nothrow {
    auto arbolDePrueba = crearArbolDePrueba();
    mostrarArbol(arbolDePrueba);
}
```

Este código en D crea un árbol binario, que es una estructura de datos que almacena datos en nodos, donde cada nodo puede tener un nodo hijo izquierdo y un nodo hijo derecho.

El código primero define la estructura `Bintree` que es una estructura inmutable, lo que significa que los datos no se pueden modificar una vez que se crean. La estructura tiene tres miembros: `left`, `right` y `value`. Los miembros `left` y `right` son punteros a dos nodos hijos, y el miembro `value` es el valor del nodo actual.

A continuación, el código define dos alias, `Bintree!L` y `Bintree!R`, que son alias para `Bintree!(immutable)`. Esto se hace para hacer más fácil el trabajo con nodos hijos izquierdos y derechos.

La función `crearArbolDePrueba` crea un árbol binario de prueba. El árbol tiene tres nodos: un nodo raíz con el valor 200, un nodo hijo izquierdo con el valor 300 y un nodo hijo derecho con el valor 400.

La función `mostrarArbol` recorre el árbol binario y muestra el valor de cada nodo. La función usa la recursión para recorrer el árbol.

La función `main` crea el árbol binario de prueba y luego lo muestra usando la función `mostrarArbol`.