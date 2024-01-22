```c#
using System;
using System.Collections.Generic;
using System.Linq;

namespace CodigoComplejoCSharp
{
    class Program
    {
        static void Main(string[] args)
        {
            // Definir una clase genérica para representar un árbol binario de búsqueda
            class ArbolBinarioBusqueda<T> where T : IComparable<T>
            {
                private Nodo<T> _raiz;

                // Clase interna para representar un nodo del árbol
                class Nodo<T>
                {
                    public T Valor { get; set; }
                    public Nodo<T> Izquierdo { get; set; }
                    public Nodo<T> Derecho { get; set; }

                    public Nodo(T valor)
                    {
                        Valor = valor;
                    }
                }

                // Insertar un valor en el árbol
                public void Insertar(T valor)
                {
                    if (_raiz == null)
                    {
                        _raiz = new Nodo<T>(valor);
                    }
                    else
                    {
                        Insertar(_raiz, valor);
                    }
                }

                private void Insertar(Nodo<T> nodoActual, T valor)
                {
                    if (valor.CompareTo(nodoActual.Valor) < 0)
                    {
                        if (nodoActual.Izquierdo == null)
                        {
                            nodoActual.Izquierdo = new Nodo<T>(valor);
                        }
                        else
                        {
                            Insertar(nodoActual.Izquierdo, valor);
                        }
                    }
                    else
                    {
                        if (nodoActual.Derecho == null)
                        {
                            nodoActual.Derecho = new Nodo<T>(valor);
                        }
                        else
                        {
                            Insertar(nodoActual.Derecho, valor);
                        }
                    }
                }

                // Buscar un valor en el árbol
                public bool Buscar(T valor)
                {
                    return Buscar(_raiz, valor);
                }

                private bool Buscar(Nodo<T> nodoActual, T valor)
                {
                    if (nodoActual == null)
                    {
                        return false;
                    }
                    else if (valor.CompareTo(nodoActual.Valor) == 0)
                    {
                        return true;
                    }
                    else if (valor.CompareTo(nodoActual.Valor) < 0)
                    {
                        return Buscar(nodoActual.Izquierdo, valor);
                    }
                    else
                    {
                        return Buscar(nodoActual.Derecho, valor);
                    }
                }

                // Eliminar un valor del árbol
                public bool Eliminar(T valor)
                {
                    return Eliminar(_raiz, valor);
                }

                private bool Eliminar(Nodo<T> nodoActual, T valor)
                {
                    if (nodoActual == null)
                    {
                        return false;
                    }
                    else if (valor.CompareTo(nodoActual.Valor) == 0)
                    {
                        // Caso 1: Nodo sin hijos
                        if (nodoActual.Izquierdo == null && nodoActual.Derecho == null)
                        {
                            EliminarNodo(nodoActual);
                        }
                        // Caso 2: Nodo con un solo hijo
                        else if (nodoActual.Izquierdo != null && nodoActual.Derecho == null)
                        {
                            ReemplazarNodo(nodoActual, nodoActual.Izquierdo);
                        }
                        else if (nodoActual.Izquierdo == null && nodoActual.Derecho != null)
                        {
                            ReemplazarNodo(nodoActual, nodoActual.Derecho);
                        }
                        // Caso 3: Nodo con dos hijos
                        else
                        {
                            // Encontrar el nodo más pequeño del subárbol derecho
                            Nodo<T> nodoReemplazo = nodoActual.Derecho;
                            while (nodoReemplazo.Izquierdo != null)
                            {
                                nodoReemplazo = nodoReemplazo.Izquierdo;
                            }

                            // Reemplazar el valor del nodo actual con el valor del nodo de reemplazo
                            nodoActual.Valor = nodoReemplazo.Valor;

                            // Eliminar el nodo de reemplazo
                            EliminarNodo(nodoReemplazo);
                        }

                        return true;
                    }
                    else if (valor.CompareTo(nodoActual.Valor) < 0)
                    {
                        return Eliminar(nodoActual.Izquierdo, valor);
                    }
                    else
                    {
                        return Eliminar(nodoActual.Derecho, valor);
                    }
                }

                private void EliminarNodo(Nodo<T> nodo)
                {
                    // Si el nodo no tiene hijos, simplemente eliminarlo
                    if (nodo.Izquierdo == null && nodo.Derecho == null)
                    {
                        nodo = null;
                    }
                    // Si el nodo tiene un solo hijo, reemplazarlo por el hijo
                    else if (nodo.Izquierdo != null && nodo.Derecho == null)
                    {
                        nodo = nodo.Izquierdo;
                    }
                    else if (nodo.Izquierdo == null && nodo.Derecho != null)
                    {
                        nodo = nodo.Derecho;
                    }
                }

                private void ReemplazarNodo(Nodo<T> nodo, Nodo<T> nodoReemplazo)
                {
                    nodo.Valor = nodoReemplazo.Valor;
                    nodo.Izquierdo = nodoReemplazo.Izquierdo;
                    nodo.Derecho = nodoReemplazo.Derecho;
                }
            }

            // Crear un árbol binario de búsqueda
            ArbolBinarioBusqueda<int> arbol = new ArbolBinarioBusqueda<int>();

            // Insertar algunos valores en el árbol
            arbol.Insertar(10);
            arbol.Insertar(5);
            arbol.Insertar(15);
            arbol.Insertar(2);
            arbol.Insertar(7);
            arbol.Insertar(12);
            arbol.Insertar(20);

            // Buscar un valor en el árbol
            bool encontrado = arbol.Buscar(10);
            Console.WriteLine($"El valor 10 {(encontrado ? "se encontró" : "no se encontró")} en el árbol.");

            // Eliminar un valor del árbol
            bool eliminado = arbol.Eliminar(15);
            Console.WriteLine($"El valor 15 {(eliminado ? "se eliminó" : "no se eliminó")} del árbol.");

            // Recorrer el árbol en orden
            Console.WriteLine("Recorrer el árbol en orden:");
            RecorrerEnOrden(arbol._raiz);

            // Recorrer el árbol en preorden
            Console.WriteLine("Recorrer el árbol en preorden:");
            RecorrerEnPreorden(arbol._raiz);

            // Recorrer el árbol en postorden
            Console.WriteLine("Recorrer el árbol en postorden:");
            RecorrerEnPostorden(arbol._raiz);
        }

        // Método para recorrer el árbol en orden
        private static void RecorrerEnOrden(Nodo<int> nodo)
        {
            if (nodo != null)
            {
                RecorrerEnOrden(nodo.Izquierdo);
                Console.WriteLine(nodo.Valor);
                RecorrerEnOrden(nodo.Derecho);
            }
        }

        // Método para recorrer el árbol en preorden
        private static void RecorrerEnPreorden(Nodo<int> nodo)
        {
            if (nodo != null)
            {
                Console.WriteLine(nodo.Valor);
                RecorrerEnPreorden(nodo.Izquierdo);
                RecorrerEnPreorden(nodo.Derecho);
            }
        }

        // Método para recorrer el árbol en postorden
        private static void RecorrerEnPostorden(Nodo<int> nodo)
        {
            if (nodo != null)
            {
                RecorrerEnPostorden(nodo.Izquierdo);
                RecorrerEnPostorden(nodo.Derecho);
                Console.WriteLine(nodo.Valor);
            }
        }
    }
}
```

Este código implementa un árbol binario de búsqueda en C#. Un árbol binario de búsqueda es una estructura de datos de árbol que mantiene los datos ordenados.

El código incluye las siguientes características:

* Una clase genérica `ArbolBinarioBusqueda<T>` que representa un árbol binario de búsqueda de elementos de tipo `T`, que debe implementar la interfaz `IComparable<T>` para poder comparar los elementos.
* Un constructor que inicializa el árbol vacío.
* Un método `Insertar` que inserta un valor en el árbol.
* Un método `Buscar` que busca un valor en el árbol.
* Un método `Eliminar` que elimina un valor del árbol.
* Tres métodos que recorren el árbol en orden, preorden y postorden.

El código se puede utilizar para implementar una variedad de algoritmos y estructuras de datos basados en árboles binarios de búsqueda.