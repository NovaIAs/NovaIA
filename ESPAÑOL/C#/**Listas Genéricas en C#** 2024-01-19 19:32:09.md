```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

class Program
{
    static void Main(string[] args)
    {
        // Definición de una clase genérica
        public class Lista<T>
        {
            // Campo privado para almacenar los elementos de la lista
            private T[] elementos;

            // Constructor que inicializa la lista con un tamaño específico
            public Lista(int tamaño)
            {
                elementos = new T[tamaño];
            }

            // Propiedad que devuelve el número de elementos de la lista
            public int Longitud
            {
                get { return elementos.Length; }
            }

            // Método para agregar un elemento a la lista
            public void Agregar(T elemento)
            {
                // Si la lista está llena, se duplica su tamaño
                if (Longitud == elementos.Length)
                {
                    Array.Resize(ref elementos, elementos.Length * 2);
                }

                // Se agrega el elemento al final de la lista
                elementos[Longitud] = elemento;
            }

            // Método para obtener un elemento de la lista por su índice
            public T Obtener(int índice)
            {
                // Se verifica que el índice sea válido
                if (índice < 0 || índice >= Longitud)
                {
                    throw new IndexOutOfRangeException();
                }

                // Se devuelve el elemento correspondiente al índice
                return elementos[índice];
            }

            // Método para eliminar un elemento de la lista por su índice
            public void Eliminar(int índice)
            {
                // Se verifica que el índice sea válido
                if (índice < 0 || índice >= Longitud)
                {
                    throw new IndexOutOfRangeException();
                }

                // Se desplazan los elementos siguientes al elemento a eliminar una posición hacia delante
                for (int i = índice + 1; i < Longitud; i++)
                {
                    elementos[i - 1] = elementos[i];
                }

                // Se reduce el tamaño de la lista en una unidad
                Array.Resize(ref elementos, elementos.Length - 1);
            }

            // Método para insertar un elemento en la lista en una posición específica
            public void Insertar(int índice, T elemento)
            {
                // Se verifica que el índice sea válido
                if (índice < 0 || índice > Longitud)
                {
                    throw new IndexOutOfRangeException();
                }

                // Si la lista está llena, se duplica su tamaño
                if (Longitud == elementos.Length)
                {
                    Array.Resize(ref elementos, elementos.Length * 2);
                }

                // Se desplazan los elementos siguientes al índice de inserción una posición hacia atrás
                for (int i = Longitud; i > índice; i--)
                {
                    elementos[i] = elementos[i - 1];
                }

                // Se inserta el elemento en la posición especificada
                elementos[índice] = elemento;
            }

            // Método para buscar un elemento en la lista y devolver su índice
            public int Buscar(T elemento)
            {
                // Se recorre la lista buscando el elemento
                for (int i = 0; i < Longitud; i++)
                {
                    if (elementos[i].Equals(elemento))
                    {
                        return i;
                    }
                }

                // Si el elemento no se encontró, se devuelve -1
                return -1;
            }

            // Método para ordenar la lista en orden ascendente
            public void Ordenar()
            {
                // Se utiliza el algoritmo de ordenamiento por burbuja
                for (int i = 0; i < Longitud - 1; i++)
                {
                    for (int j = i + 1; j < Longitud; j++)
                    {
                        if (elementos[i].CompareTo(elementos[j]) > 0)
                        {
                            T tmp = elementos[i];
                            elementos[i] = elementos[j];
                            elementos[j] = tmp;
                        }
                    }
                }
            }

            // Método para imprimir la lista en la consola
            public void Imprimir()
            {
                // Se recorre la lista y se imprime cada elemento
                for (int i = 0; i < Longitud; i++)
                {
                    Console.Write(elementos[i] + " ");
                }

                // Se imprime una nueva línea al final de la lista
                Console.WriteLine();
            }
        }

        // Creación de una lista de enteros
        Lista<int> listaEnteros = new Lista<int>(10);

        // Adición de elementos a la lista
        listaEnteros.Agregar(1);
        listaEnteros.Agregar(3);
        listaEnteros.Agregar(5);
        listaEnteros.Agregar(7);
        listaEnteros.Agregar(9);

        // Impresión de la lista
        Console.WriteLine("Lista de enteros:");
        listaEnteros.Imprimir();

        // Obtención de un elemento de la lista por su índice
        int elementoObtenido = listaEnteros.Obtener(2);
        Console.WriteLine("Elemento obtenido por su índice: " + elementoObtenido);

        // Eliminación de un elemento de la lista por su índice
        listaEnteros.Eliminar(3);
        Console.WriteLine("Lista después de eliminar un elemento:");
        listaEnteros.Imprimir();

        // Inserción de un elemento en la lista en una posición específica
        listaEnteros.Insertar(2, 4);
        Console.WriteLine("Lista después de insertar un elemento:");
        listaEnteros.Imprimir();

        // Búsqueda de un elemento en la lista y devolución de su índice
        int índiceEncontrado = listaEnteros.Buscar(4);
        Console.WriteLine("Índice del elemento buscado: " + índiceEncontrado);

        // Ordenamiento de la lista en orden ascendente
        listaEnteros.Ordenar();
        Console.WriteLine("Lista después de ordenarla:");
        listaEnteros.Imprimir();

        // Creación de una lista de cadenas
        Lista<string> listaCadenas = new Lista<string>(10);

        // Adición de elementos a la lista
        listaCadenas.Agregar("Hola");
        listaCadenas.Agregar("Mundo");
        listaCadenas.Agregar("!");

        // Impresión de la lista
        Console.WriteLine("Lista de cadenas:");
        listaCadenas.Imprimir();

        // Obtención de un elemento de la lista por su índice
        string elementoCadenaObtenido = listaCadenas.Obtener(1);
        Console.WriteLine("Elemento cadena obtenido por su índice: " + elementoCadenaObtenido);

        // Eliminación de un elemento de la lista por su índice
        listaCadenas.Eliminar(2);
        Console.WriteLine("Lista después de eliminar un elemento:");
        listaCadenas.Imprimir();

        // Inserción de un elemento en la lista en una posición específica
        listaCadenas.Insertar(1, "Bienvenidos");
        Console.WriteLine("Lista después de insertar un elemento:");
        listaCadenas.Imprimir();

        // Búsqueda de un elemento en la lista y devolución de su índice
        int índiceEncontradoCadena = listaCadenas.Buscar("Bienvenidos");
        Console.WriteLine("Índice del elemento cadena buscado: " + índiceEncontradoCadena);

        // Ordenamiento de la lista en orden ascendente
        listaCadenas.Ordenar();
        Console.WriteLine("Lista después de ordenarla:");
        listaCadenas.Imprimir();

        // Esperar a que el usuario pulse una tecla para salir
        Console.ReadKey();
    }
}
```

Explicación del código:

* Se define una clase genérica `Lista<T>` que representa una lista genérica de elementos. Esta clase tiene los siguientes miembros:
    * Un campo privado `elementos` que almacena los elementos de la lista.
    * Un constructor que inicializa la lista con un tamaño específico.
    * Una propiedad `Longitud` que devuelve el número de elementos de la lista.
    * Un método `Agregar()` que agrega un elemento a la lista.
    * Un método `Obtener()` que obtiene un elemento de la lista por su índice.
    * Un método `Eliminar()` que elimina un elemento de la lista por su índice.
    * Un método `Insertar()` que inserta un elemento en la lista en una posición específica.
    * Un método `Buscar()` que busca un elemento en la lista y devuelve su índice.
    * Un método `Ordenar()` que ordena la lista en orden ascendente.
    * Un método `Imprimir()` que imprime la lista en la consola.

* Se crean dos listas, una de enteros y otra de cadenas.

* Se agregan elementos a las listas.

* Se imprimen las listas.

* Se obtiene un elemento de cada lista por su índice.

* Se elimina un elemento de cada lista por su índice.

* Se inserta un elemento en cada lista en una posición específica.

* Se busca un elemento en cada lista y se devuelve su índice.

* Se ordenan las listas en orden ascendente.

* Se imprimen las listas.

Este código es un ejemplo de cómo utilizar listas genéricas en C#. Las listas genéricas son una herramienta poderosa que se puede utilizar para almacenar y manipular datos de cualquier tipo.