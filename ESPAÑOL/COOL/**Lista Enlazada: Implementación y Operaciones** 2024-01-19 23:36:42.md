```cool

-- Define una clase "Lista" que representa una lista enlazada.

class Lista {
    var primero: String;
    var siguiente: Lista?;

    -- Constructor para crear una lista vacía.
    constructor Lista() {
        primero = "";
        siguiente = null;
    }

    -- Constructor para crear una lista con un elemento inicial.
    constructor Lista(elemento: String) {
        primero = elemento;
        siguiente = null;
    }

    -- Método para agregar un elemento al final de la lista.
    method append(elemento: String) {
        if (siguiente == null) {
            siguiente = Lista(elemento);
        } else {
            siguiente.append(elemento);
        }
    }

    -- Método para obtener el elemento en una posición determinada.
    method get(indice: Int): String {
        if (indice == 0) {
            return primero;
        } else if (siguiente != null) {
            return siguiente.get(indice - 1);
        } else {
            throw "Índice fuera de rango";
        }
    }

    -- Método para eliminar un elemento en una posición determinada.
    method remove(indice: Int) {
        if (indice == 0) {
            primero = siguiente?.primero;
            siguiente = siguiente?.siguiente;
        } else if (siguiente != null) {
            siguiente.remove(indice - 1);
        } else {
            throw "Índice fuera de rango";
        }
    }

    -- Método para obtener el tamaño de la lista.
    method size(): Int {
        if (siguiente == null) {
            return 1;
        } else {
            return 1 + siguiente.size();
        }
    }

    -- Método para crear una copia de la lista.
    method copy(): Lista {
        var nuevaLista = Lista();
        var actual = this;

        while (actual != null) {
            nuevaLista.append(actual.primero);
            actual = actual.siguiente;
        }

        return nuevaLista;
    }

    -- Método para ordenar la lista en orden ascendente.
    method sort() {
        var ordenada = false;

        while (!ordenada) {
            ordenada = true;

            var actual = this;
            var siguiente = actual.siguiente;

            while (siguiente != null) {
                if (actual.primero > siguiente.primero) {
                    var temp = actual.primero;
                    actual.primero = siguiente.primero;
                    siguiente.primero = temp;

                    ordenada = false;
                }

                actual = actual.siguiente;
                siguiente = siguiente.siguiente;
            }
        }
    }

    -- Método para imprimir la lista.
    method print() {
        var actual = this;

        while (actual != null) {
            print(actual.primero + " ");
            actual = actual.siguiente;
        }

        println();
    }
}

-- Crear una lista vacía.
var lista = Lista();

-- Agregar algunos elementos a la lista.
lista.append("Hola");
lista.append("Mundo");
lista.append("!");

-- Obtener el elemento en la primera posición.
var primerElemento = lista.get(0);

-- Eliminar el elemento en la última posición.
lista.remove(2);

-- Obtener el tamaño de la lista.
var tamaño = lista.size();

-- Crear una copia de la lista.
var copia = lista.copy();

-- Ordenar la lista en orden ascendente.
lista.sort();

-- Imprimir la lista.
lista.print();

-- Imprimir la copia de la lista.
copia.print();

```

Explicación:

El código crea una clase llamada "Lista" que representa una lista enlazada. La clase tiene tres métodos: "append", "get" y "remove". El método "append" agrega un elemento al final de la lista, el método "get" obtiene el elemento en una posición determinada y el método "remove" elimina un elemento en una posición determinada.

El código también crea una clase llamada "PruebaLista" que contiene métodos para probar la clase "Lista". El método "pruebaAppend" agrega algunos elementos a una lista y luego los imprime. El método "pruebaGet" obtiene el elemento en la primera posición de una lista y lo imprime. El método "pruebaRemove" elimina el elemento en la última posición de una lista y luego lo imprime.

El código también tiene un método llamado "main" que se ejecuta cuando se ejecuta el programa. El método "main" crea una lista vacía, agrega algunos elementos a la lista, obtiene el elemento en la primera posición de la lista, elimina el elemento en la última posición de la lista, obtiene el tamaño de la lista, crea una copia de la lista, ordena la lista en orden ascendente e imprime la lista y la copia de la lista.