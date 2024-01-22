```c#
// Implementación de un algoritmo de clasificación por ordenación rápida en C#.

// Definir una clase para un nodo de una lista enlazada.
public class Nodo
{
    public int valor;
    public Nodo siguiente;

    public Nodo(int valor)
    {
        this.valor = valor;
        this.siguiente = null;
    }
}

// Definir una clase para una lista enlazada.
public class ListaEnlazada
{
    private Nodo cabeza;

    // Método para insertar un nodo al final de la lista.
    public void InsertarAlFinal(int valor)
    {
        if (cabeza == null)
        {
            cabeza = new Nodo(valor);
        }
        else
        {
            Nodo nodoActual = cabeza;
            while (nodoActual.siguiente != null)
            {
                nodoActual = nodoActual.siguiente;
            }
            nodoActual.siguiente = new Nodo(valor);
        }
    }

    // Método para ordenar la lista utilizando el algoritmo de ordenación rápida.
    public void Ordenar()
    {
        OrdenarRecursivo(cabeza);
    }

    // Método recursivo para ordenar la lista.
    private void OrdenarRecursivo(Nodo nodoActual)
    {
        if (nodoActual.siguiente == null)
        {
            return;
        }

        Nodo pivote = nodoActual;
        Nodo nodoAnterior = null;
        Nodo nodoAuxiliar = nodoActual.siguiente;

        while (nodoAuxiliar != null)
        {
            if (nodoAuxiliar.valor < pivote.valor)
            {
                if (nodoAnterior == null)
                {
                    nodoActual.siguiente = nodoAuxiliar.siguiente;
                    nodoAuxiliar.siguiente = pivote;
                    pivote = nodoAuxiliar;
                }
                else
                {
                    nodoAnterior.siguiente = nodoAuxiliar;
                    nodoActual.siguiente = nodoAuxiliar.siguiente;
                    nodoAuxiliar.siguiente = pivote;
                    pivote = nodoAuxiliar;
                    nodoAnterior = nodoAnterior.siguiente;
                }
            }
            else
            {
                nodoAnterior = nodoActual;
                nodoActual = nodoActual.siguiente;
            }
            nodoAuxiliar = nodoActual.siguiente;
        }

        OrdenarRecursivo(pivote.siguiente);
        OrdenarRecursivo(cabeza);
    }

    // Método para imprimir la lista.
    public void Imprimir()
    {
        Nodo nodoActual = cabeza;
        while (nodoActual != null)
        {
            Console.Write(nodoActual.valor + " ");
            nodoActual = nodoActual.siguiente;
        }
        Console.WriteLine();
    }
}

// Crear una lista enlazada y agregar algunos valores.
ListaEnlazada lista = new ListaEnlazada();
lista.InsertarAlFinal(10);
lista.InsertarAlFinal(5);
lista.InsertarAlFinal(20);
lista.InsertarAlFinal(7);
lista.InsertarAlFinal(3);

// Ordenar la lista.
lista.Ordenar();

// Imprimir la lista ordenada.
lista.Imprimir();
```

Explicación del código:

* La clase `Nodo` representa un nodo de una lista enlazada.
* La clase `ListaEnlazada` representa una lista enlazada y proporciona métodos para insertar nodos, ordenar la lista y imprimirla.
* El método `InsertarAlFinal` agrega un nuevo nodo al final de la lista.
* El método `Ordenar` ordena la lista utilizando el algoritmo de ordenación rápida de forma recursiva.
* El método `OrdenarRecursivo` es una implementación recursiva del algoritmo de ordenación rápida.
* El método `Imprimir` imprime la lista en la consola.

El algoritmo de ordenación rápida funciona de la siguiente manera:

1. Se elige un pivote, que es un elemento de la lista.
2. La lista se divide en dos sublistas: una con los elementos menores que el pivote y otra con los elementos mayores o iguales que el pivote.
3. Se ordenan las dos sublistas de forma recursiva.
4. Se concatenan las dos sublistas ordenadas.

El algoritmo de ordenación rápida tiene una complejidad media de O(n log n), donde n es el número de elementos de la lista. Sin embargo, en el peor caso, la complejidad puede ser de O(n^2).