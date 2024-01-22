```cool
clase {ListaEnlazada} {
    atributo {nodo} cabeza;
    atributo {nodo} cola;
    atributo {entero} tamaño;

    constructor {
        cabeza ← nulo;
        cola ← nulo;
        tamaño ← 0;
    }

    método {añadir(valor)} {
        nodo nuevo ← crearNodo(valor);
        si cabeza = nulo entonces
            cabeza ← nuevo;
            cola ← nuevo;
        si no
            cola.siguiente ← nuevo;
            cola ← nuevo;
        tamaño ← tamaño + 1;
    }

    método {quitar()} {
        si cabeza = nulo entonces
            cola ← nulo;
            tamaño ← 0;
        si no
            cabeza ← cabeza.siguiente;
            tamaño ← tamaño - 1;
    }

    método {obtener(índice)} {
        nodo actual ← cabeza;
        entero contador ← 0;

        mientras contador < índice y actual ≠ nulo haz
            contador ← contador + 1;
            actual ← actual.siguiente;
        fin mientras

        si contador = índice entonces
            devolver actual.valor;
        si no
            devolver nulo;
    }

    método {tamaño()} {
        devolver tamaño;
    }

    método {vacío()} {
        devolver tamaño = 0;
    }

    método privado {crearNodo(valor)} {
        nodo nuevo ← crear nuevo nodo;
        nuevo.valor ← valor;
        nuevo.siguiente ← nulo;
        devolver nuevo;
    }
}

clase {Nodo} {
    atributo {entero} valor;
    atributo {nodo} siguiente;

    constructor {
        valor ← 0;
        siguiente ← nulo;
    }
}

clase {Main} {
    método {principal()} {
        ListaEnlazada lista ← crear nueva ListaEnlazada;

        lista.añadir(10);
        lista.añadir(20);
        lista.añadir(30);

        mientras no lista.vacío() haz
            entero valor ← lista.quitar();
            escribir(valor);
            escribirln();
        fin mientras
    }
}
```

Explicación del código:

* La clase `ListaEnlazada` representa una lista enlazada simple. Tiene los atributos `cabeza`, `cola` y `tamaño`.
* El método `constructor` inicializa los atributos de la lista enlazada.
* El método `añadir` añade un nuevo valor a la lista enlazada. Si la lista está vacía, el nuevo valor se convierte en la cabeza y la cola de la lista. Si la lista no está vacía, el nuevo valor se añade al final de la lista y se convierte en la nueva cola.
* El método `quitar` quita el primer valor de la lista enlazada. Si la lista está vacía, el método no hace nada. Si la lista no está vacía, el segundo valor de la lista se convierte en la nueva cabeza.
* El método `obtener` devuelve el valor en la posición especificada por el índice. Si el índice es válido, el método devuelve el valor. Si el índice no es válido, el método devuelve `nulo`.
* El método `tamaño` devuelve el tamaño de la lista enlazada.
* El método `vacío` devuelve `true` si la lista enlazada está vacía y `false` si no está vacía.
* La clase `Nodo` representa un nodo de la lista enlazada. Tiene los atributos `valor` y `siguiente`.
* El método `constructor` inicializa los atributos del nodo.
* La clase `Main` es la clase principal del programa.
* El método `principal` es el punto de entrada del programa. Crea una nueva lista enlazada, añade algunos valores a la lista y luego imprime los valores de la lista en la consola.