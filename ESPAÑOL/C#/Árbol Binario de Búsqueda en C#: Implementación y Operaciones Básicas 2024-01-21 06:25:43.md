```c#
using System;
using System.Collections.Generic;
using System.Linq;

namespace CodigoComplejo
{
    class Program
    {
        static void Main(string[] args)
        {
            // Definir una clase genérica para representar nodos en un árbol binario
            public class Nodo<T>
            {
                public T Valor { get; set; }
                public Nodo<T> Izquierdo { get; set; }
                public Nodo<T> Derecho { get; set; }

                public Nodo(T valor)
                {
                    Valor = valor;
                    Izquierdo = null;
                    Derecho = null;
                }
            }

            // Crear un árbol binario de búsqueda
            Nodo<int> raiz = new Nodo<int>(10);
            raiz.Izquierdo = new Nodo<int>(5);
            raiz.Derecho = new Nodo<int>(15);
            raiz.Izquierdo.Izquierdo = new Nodo<int>(2);
            raiz.Izquierdo.Derecho = new Nodo<int>(7);
            raiz.Derecho.Izquierdo = new Nodo<int>(12);
            raiz.Derecho.Derecho = new Nodo<int>(20);

            // Encontrar el menor elemento en el árbol binario de búsqueda
            Nodo<int> minimo = EncontrarMinimo(raiz);
            Console.WriteLine("El elemento mínimo del árbol es: " + minimo.Valor);

            // Encontrar el mayor elemento en el árbol binario de búsqueda
            Nodo<int> maximo = EncontrarMaximo(raiz);
            Console.WriteLine("El elemento máximo del árbol es: " + maximo.Valor);

            // Insertar un nuevo elemento en el árbol binario de búsqueda
            Insertar(raiz, 8);
            Console.WriteLine("Nuevo árbol:");
            ImprimirArbol(raiz);

            // Eliminar un elemento del árbol binario de búsqueda
            Eliminar(raiz, 5);
            Console.WriteLine("Nuevo árbol:");
            ImprimirArbol(raiz);

            // Buscar un elemento en el árbol binario de búsqueda
            Nodo<int> nodo = Buscar(raiz, 12);
            if (nodo != null)
            {
                Console.WriteLine("El elemento buscado fue encontrado");
            }
            else
            {
                Console.WriteLine("El elemento buscado no fue encontrado");
            }

            // Recorrer el árbol binario de búsqueda en orden ascendente
            Console.WriteLine("Recorrido del árbol en orden ascendente:");
            RecorrerEnOrden(raiz);

            // Recorrer el árbol binario de búsqueda en orden descendente
            Console.WriteLine("Recorrido del árbol en orden descendente:");
            RecorrerEnOrdenDescendiente(raiz);

            // Recorrer el árbol binario de búsqueda en orden transversal
            Console.WriteLine("Recorrido del árbol en orden transversal:");
            RecorrerEnOrdenTransversal(raiz);

            // Calcular la altura del árbol binario de búsqueda
            int altura = CalcularAltura(raiz);
            Console.WriteLine("La altura del árbol es: " + altura);
        }

        // Método para encontrar el menor elemento en un árbol binario de búsqueda
        public static Nodo<T> EncontrarMinimo<T>(Nodo<T> raiz)
        {
            if (raiz == null)
            {
                return null;
            }

            Nodo<T> actual = raiz;
            while (actual.Izquierdo != null)
            {
                actual = actual.Izquierdo;
            }

            return actual;
        }

        // Método para encontrar el mayor elemento en un árbol binario de búsqueda
        public static Nodo<T> EncontrarMaximo<T>(Nodo<T> raiz)
        {
            if (raiz == null)
            {
                return null;
            }

            Nodo<T> actual = raiz;
            while (actual.Derecho != null)
            {
                actual = actual.Derecho;
            }

            return actual;
        }

        // Método para insertar un nuevo elemento en un árbol binario de búsqueda
        public static void Insertar<T>(Nodo<T> raiz, T valor)
        {
            if (raiz == null)
            {
                raiz = new Nodo<T>(valor);
                return;
            }

            if (valor.CompareTo(raiz.Valor) < 0)
            {
                Insertar(raiz.Izquierdo, valor);
            }
            else
            {
                Insertar(raiz.Derecho, valor);
            }
        }

        // Método para eliminar un elemento de un árbol binario de búsqueda
        public static void Eliminar<T>(Nodo<T> raiz, T valor)
        {
            if (raiz == null)
            {
                return;
            }

            if (valor.CompareTo(raiz.Valor) < 0)
            {
                Eliminar(raiz.Izquierdo, valor);
            }
            else if (valor.CompareTo(raiz.Valor) > 0)
            {
                Eliminar(raiz.Derecho, valor);
            }
            else
            {
                // Caso 1: Nodo sin hijos
                if (raiz.Izquierdo == null && raiz.Derecho == null)
                {
                    raiz = null;
                }
                // Caso 2: Nodo con un solo hijo
                else if (raiz.Izquierdo == null)
                {
                    raiz = raiz.Derecho;
                }
                else if (raiz.Derecho == null)
                {
                    raiz = raiz.Izquierdo;
                }
                // Caso 3: Nodo con dos hijos
                else
                {
                    // Encontrar el menor elemento en el subárbol derecho
                    Nodo<T> minimo = EncontrarMinimo(raiz.Derecho);

                    // Reemplazar el valor del nodo a eliminar con el valor del menor elemento
                    raiz.Valor = minimo.Valor;

                    // Eliminar el menor elemento del subárbol derecho
                    Eliminar(raiz.Derecho, minimo.Valor);
                }
            }
        }

        // Método para buscar un elemento en un árbol binario de búsqueda
        public static Nodo<T> Buscar<T>(Nodo<T> raiz, T valor)
        {
            if (raiz == null)
            {
                return null;
            }

            if (valor.CompareTo(raiz.Valor) < 0)
            {
                return Buscar(raiz.Izquierdo, valor);
            }
            else if (valor.CompareTo(raiz.Valor) > 0)
            {
                return Buscar(raiz.Derecho, valor);
            }
            else
            {
                return raiz;