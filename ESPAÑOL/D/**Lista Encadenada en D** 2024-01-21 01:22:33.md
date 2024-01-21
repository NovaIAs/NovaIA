```d
class ListaEncadenada<T> {
    private elemento primero;
    private elemento ultimo;
    private int tamaño;

    public ListaEncadenada() {
        primero = null;
        ultimo = null;
        tamaño = 0;
    }

    public void agregar(T dato) {
        elemento nuevo = new elemento(dato);
        if (primero == null) {
            primero = nuevo;
            ultimo = nuevo;
        } else {
            ultimo.siguiente = nuevo;
            ultimo = nuevo;
        }
        tamaño++;
    }

    public T eliminarPrimero() {
        if (primero == null) {
            return null;
        }
        T dato = primero.dato;
        primero = primero.siguiente;
        if (primero == null) {
            ultimo = null;
        }
        tamaño--;
        return dato;
    }

    public T eliminarUltimo() {
        if (ultimo == null) {
            return null;
        }
        T dato = ultimo.dato;
        if (primero == ultimo) {
            primero = null;
            ultimo = null;
        } else {
            elemento anterior = primero;
            while (anterior.siguiente != ultimo) {
                anterior = anterior.siguiente;
            }
            anterior.siguiente = null;
            ultimo = anterior;
        }
        tamaño--;
        return dato;
    }

    public T eliminar(T dato) {
        if (primero == null) {
            return null;
        }
        if (primero.dato.equals(dato)) {
            return eliminarPrimero();
        }
        elemento anterior = primero;
        elemento actual = primero.siguiente;
        while (actual != null) {
            if (actual.dato.equals(dato)) {
                anterior.siguiente = actual.siguiente;
                if (actual == ultimo) {
                    ultimo = anterior;
                }
                tamaño--;
                return dato;
            }
            anterior = actual;
            actual = actual.siguiente;
        }
        return null;
    }

    public T buscar(T dato) {
        if (primero == null) {
            return null;
        }
        elemento actual = primero;
        while (actual != null) {
            if (actual.dato.equals(dato)) {
                return actual.dato;
            }
            actual = actual.siguiente;
        }
        return null;
    }

    public int tamaño() {
        return tamaño;
    }

    public boolean estáVacía() {
        return tamaño == 0;
    }

    public void imprimir() {
        if (primero == null) {
            System.out.println("La lista está vacía.");
        } else {
            elemento actual = primero;
            while (actual != null) {
                System.out.print(actual.dato + " ");
                actual = actual.siguiente;
            }
            System.out.println();
        }
    }

    private class elemento {
        private T dato;
        private elemento siguiente;

        public elemento(T dato) {
            this.dato = dato;
            siguiente = null;
        }
    }
}

public class Main {
    public static void main(String[] args) {
        ListaEncadenada<Integer> lista = new ListaEncadenada<>();

        lista.agregar(1);
        lista.agregar(2);
        lista.agregar(3);
        lista.agregar(4);
        lista.agregar(5);

        lista.imprimir(); // Imprime: 1 2 3 4 5

        lista.eliminarPrimero();
        lista.eliminarUltimo();

        lista.imprimir(); // Imprime: 2 3 4

        lista.eliminar(3);

        lista.imprimir(); // Imprime: 2 4

        System.out.println("El tamaño de la lista es: " + lista.tamaño()); // Imprime: 2

        System.out.println("¿La lista está vacía? " + lista.estáVacía()); // Imprime: false

        System.out.println("El primer elemento de la lista es: " + lista.primero.dato); // Imprime: 2

        System.out.println("El último elemento de la lista es: " + lista.ultimo.dato); // Imprime: 4

        System.out.println("El elemento 3 existe en la lista? " + lista.buscar(3)); // Imprime: null

        System.out.println("El elemento 4 existe en la lista? " + lista.buscar(4)); // Imprime: 4
    }
}
```

Este código crea una lista enlazada en D, que es una estructura de datos que almacena una colección de elementos en nodos que están enlazados entre sí. Cada nodo tiene un dato y un puntero al siguiente nodo.

La clase `ListaEncadenada` tiene los siguientes métodos:

* `agregar`: agrega un nuevo elemento al final de la lista.
* `eliminarPrimero`: elimina el primer elemento de la lista.
* `eliminarUltimo`: elimina el último elemento de la lista.
* `eliminar`: elimina el primer elemento que coincida con el dato especificado.
* `buscar`: busca el primer elemento que coincida con el dato especificado.
* `tamaño`: devuelve el número de elementos en la lista.
* `estáVacía`: devuelve `true` si la lista está vacía, y `false` de lo contrario.
* `imprimir`: imprime los elementos de la lista en la consola.

La clase `elemento` representa un solo nodo de la lista enlazada. Tiene los siguientes miembros:

* `dato`: el dato almacenado en el nodo.
* `siguiente`: un puntero al siguiente nodo en la lista.

La clase `Main` es el punto de entrada del programa. Crea una lista enlazada, agrega algunos elementos a ella, y luego llama a los métodos de la lista para eliminar, buscar, y imprimir los elementos. También imprime el tamaño de la lista, si está vacía o no, y el primer y último elemento de la lista.