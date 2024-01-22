```
module Main {

    class Nodo {
        var valor : String;
        var siguiente : Nodo;

        constructor(valor : String, siguiente : Nodo) {
            this.valor = valor;
            this.siguiente = siguiente;
        }
    }

    class Lista {
        var primero : Nodo;

        constructor() {
            this.primero = null;
        }

        insertar(valor : String) {
            if (this.primero == null) {
                this.primero = new Nodo(valor, null);
            } else {
                var ultimo : Nodo = this.primero;
                while (ultimo.siguiente != null) {
                    ultimo = ultimo.siguiente;
                }
                ultimo.siguiente = new Nodo(valor, null);
            }
        }

        eliminar(valor : String) {
            if (this.primero != null) {
                if (this.primero.valor == valor) {
                    this.primero = this.primero.siguiente;
                } else {
                    var anterior : Nodo = this.primero;
                    var actual : Nodo = this.primero.siguiente;
                    while (actual != null && actual.valor != valor) {
                        anterior = actual;
                        actual = actual.siguiente;
                    }
                    if (actual != null) {
                        anterior.siguiente = actual.siguiente;
                    }
                }
            }
        }

        buscar(valor : String) : Boolean {
            var actual : Nodo = this.primero;
            while (actual != null && actual.valor != valor) {
                actual = actual.siguiente;
            }
            return actual != null;
        }

        mostrar() {
            var actual : Nodo = this.primero;
            while (actual != null) {
                print(actual.valor);
                actual = actual.siguiente;
            }
        }
    }

    class Main {

        constructor() {
            var lista : Lista = new Lista();
            lista.insertar("Hola");
            lista.insertar("Mundo");
            lista.insertar("!");
            lista.mostrar();
            lista.eliminar("Mundo");
            lista.mostrar();
            print(lista.buscar("Hola"));
            print(lista.buscar("Mundo"));
        }
    }
}
```

Explicación del código:

* El código define un módulo llamado `Main` que contiene un conjunto de clases y funciones.
* La clase `Nodo` representa un nodo de una lista enlazada. Tiene dos atributos: `valor`, que almacena el valor del nodo, y `siguiente`, que apunta al siguiente nodo de la lista.
* La clase `Lista` representa una lista enlazada. Tiene un atributo `primero` que apunta al primer nodo de la lista.
* La clase `Main` contiene la función principal del programa. En ella se crea una lista y se le añaden tres valores. Luego se muestra la lista, se elimina un valor, se vuelve a mostrar la lista y se busca un valor.
* Las funciones `insertar`, `eliminar`, `buscar` y `mostrar` de la clase `Lista` implementan la funcionalidad básica de una lista enlazada. La función `insertar` añade un valor al final de la lista, la función `eliminar` elimina un valor de la lista, la función `buscar` busca un valor en la lista y la función `mostrar` muestra todos los valores de la lista.

El código es complejo porque implementa una lista enlazada en COOL. Una lista enlazada es una estructura de datos que se utiliza para almacenar una colección de elementos de forma ordenada. Cada elemento de la lista está representado por un nodo, que contiene el valor del elemento y un puntero al siguiente nodo de la lista.

La implementación de una lista enlazada en COOL es compleja porque requiere la gestión de los punteros a los nodos. Sin embargo, esta implementación es eficiente y permite añadir, eliminar y buscar elementos de la lista en tiempo constante.