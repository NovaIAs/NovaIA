```java
// Clase para representar una lista enlazada simple.
class ListaEnlazada {
    private Nodo primero; // Primer nodo de la lista.
    private Nodo ultimo; // Último nodo de la lista.
    private int tamaño; // Número de elementos en la lista.

    // Constructor de la lista.
    public ListaEnlazada() {
        primero = null;
        ultimo = null;
        tamaño = 0;
    }

    // Método para añadir un elemento al final de la lista.
    public void añadir(int elemento) {
        Nodo nuevoNodo = new Nodo(elemento);
        if (primero == null) {
            primero = nuevoNodo;
            ultimo = nuevoNodo;
        } else {
            ultimo.siguiente = nuevoNodo;
            ultimo = nuevoNodo;
        }
        tamaño++;
    }

    // Método para eliminar un elemento de la lista por su valor.
    public void eliminar(int elemento) {
        if (primero == null) {
            return;
        }

        if (primero.elemento == elemento) {
            primero = primero.siguiente;
            if (primero == null) {
                ultimo = null;
            }
        } else {
            Nodo actual = primero;
            while (actual.siguiente != null) {
                if (actual.siguiente.elemento == elemento) {
                    actual.siguiente = actual.siguiente.siguiente;
                    if (actual.siguiente == null) {
                        ultimo = actual;
                    }
                    break;
                }
                actual = actual.siguiente;
            }
        }

        tamaño--;
    }

    // Método para obtener el tamaño de la lista.
    public int getTamaño() {
        return tamaño;
    }

    // Método para comprobar si la lista está vacía.
    public boolean estáVacía() {
        return tamaño == 0;
    }

    // Método para imprimir la lista en consola.
    public void imprimir() {
        Nodo actual = primero;
        while (actual != null) {
            System.out.print(actual.elemento + " ");
            actual = actual.siguiente;
        }
        System.out.println();
    }

    // Clase para representar un nodo de la lista enlazada.
    private class Nodo {
        private int elemento; // Elemento almacenado en el nodo.
        private Nodo siguiente; // Referencia al siguiente nodo de la lista.

        // Constructor del nodo.
        public Nodo(int elemento) {
            this.elemento = elemento;
            siguiente = null;
        }
    }
}

// Clase principal del programa.
public class Main {
    public static void main(String[] args) {
        // Crear una lista enlazada.
        ListaEnlazada lista = new ListaEnlazada();

        // Añadir elementos a la lista.
        lista.añadir(1);
        lista.añadir(2);
        lista.añadir(3);
        lista.añadir(4);
        lista.añadir(5);

        // Imprimir la lista.
        System.out.println("Lista:");
        lista.imprimir();

        // Eliminar un elemento de la lista.
        lista.eliminar(3);

        // Imprimir la lista después de eliminar un elemento.
        System.out.println("Lista después de eliminar el elemento 3:");
        lista.imprimir();

        // Obtener el tamaño de la lista.
        System.out.println("Tamaño de la lista:");
        System.out.println(lista.getTamaño());

        // Comprobar si la lista está vacía.
        System.out.println("¿Está la lista vacía?");
        System.out.println(lista.estáVacía());
    }
}
```

Explicación del código:

* La clase `ListaEnlazada` representa una lista enlazada simple.
* La clase `Nodo` representa un nodo de la lista enlazada.
* La clase `Main` es la clase principal del programa.
* El método `main` de la clase `Main` crea una lista enlazada, añade elementos a la lista, elimina un elemento de la lista, imprime la lista, obtiene el tamaño de la lista y comprueba si la lista está vacía.