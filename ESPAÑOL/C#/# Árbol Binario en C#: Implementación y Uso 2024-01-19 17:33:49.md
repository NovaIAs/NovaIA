```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ArbolBinario
{
    class Nodo
    {
        public int Valor { get; set; }
        public Nodo Izquierdo { get; set; }
        public Nodo Derecho { get; set; }

        public Nodo(int valor)
        {
            Valor = valor;
            Izquierdo = null;
            Derecho = null;
        }
    }

    class ArbolBinario
    {
        private Nodo _raiz;

        public ArbolBinario()
        {
            _raiz = null;
        }

        public void Insertar(int valor)
        {
            Nodo nuevoNodo = new Nodo(valor);
            if (_raiz == null)
            {
                _raiz = nuevoNodo;
            }
            else
            {
                InsertarRecursivo(_raiz, nuevoNodo);
            }
        }

        private void InsertarRecursivo(Nodo nodoActual, Nodo nuevoNodo)
        {
            if (nuevoNodo.Valor < nodoActual.Valor)
            {
                if (nodoActual.Izquierdo == null)
                {
                    nodoActual.Izquierdo = nuevoNodo;
                }
                else
                {
                    InsertarRecursivo(nodoActual.Izquierdo, nuevoNodo);
                }
            }
            else
            {
                if (nodoActual.Derecho == null)
                {
                    nodoActual.Derecho = nuevoNodo;
                }
                else
                {
                    InsertarRecursivo(nodoActual.Derecho, nuevoNodo);
                }
            }
        }

        public bool Buscar(int valor)
        {
            return BuscarRecursivo(_raiz, valor);
        }

        private bool BuscarRecursivo(Nodo nodoActual, int valor)
        {
            if (nodoActual == null)
            {
                return false;
            }
            else if (nodoActual.Valor == valor)
            {
                return true;
            }
            else if (valor < nodoActual.Valor)
            {
                return BuscarRecursivo(nodoActual.Izquierdo, valor);
            }
            else
            {
                return BuscarRecursivo(nodoActual.Derecho, valor);
            }
        }

        public void Eliminar(int valor)
        {
            EliminarRecursivo(_raiz, valor);
        }

        private Nodo EliminarRecursivo(Nodo nodoActual, int valor)
        {
            if (nodoActual == null)
            {
                return null;
            }
            else if (valor == nodoActual.Valor)
            {
                if (nodoActual.Izquierdo == null && nodoActual.Derecho == null)
                {
                    return null;
                }
                else if (nodoActual.Izquierdo == null)
                {
                    return nodoActual.Derecho;
                }
                else if (nodoActual.Derecho == null)
                {
                    return nodoActual.Izquierdo;
                }
                else
                {
                    int valorSucesor = ObtenerSucesor(nodoActual.Derecho);
                    nodoActual.Valor = valorSucesor;
                    nodoActual.Derecho = EliminarRecursivo(nodoActual.Derecho, valorSucesor);
                    return nodoActual;
                }
            }
            else if (valor < nodoActual.Valor)
            {
                nodoActual.Izquierdo = EliminarRecursivo(nodoActual.Izquierdo, valor);
                return nodoActual;
            }
            else
            {
                nodoActual.Derecho = EliminarRecursivo(nodoActual.Derecho, valor);
                return nodoActual;
            }
        }

        private int ObtenerSucesor(Nodo nodoActual)
        {
            while (nodoActual.Izquierdo != null)
            {
                nodoActual = nodoActual.Izquierdo;
            }
            return nodoActual.Valor;
        }

        public void RecorrerPreOrden()
        {
            RecorrerPreOrdenRecursivo(_raiz);
        }

        private void RecorrerPreOrdenRecursivo(Nodo nodoActual)
        {
            if (nodoActual != null)
            {
                Console.WriteLine(nodoActual.Valor);
                RecorrerPreOrdenRecursivo(nodoActual.Izquierdo);
                RecorrerPreOrdenRecursivo(nodoActual.Derecho);
            }
        }

        public void RecorrerInOrden()
        {
            RecorrerInOrdenRecursivo(_raiz);
        }

        private void RecorrerInOrdenRecursivo(Nodo nodoActual)
        {
            if (nodoActual != null)
            {
                RecorrerInOrdenRecursivo(nodoActual.Izquierdo);
                Console.WriteLine(nodoActual.Valor);
                RecorrerInOrdenRecursivo(nodoActual.Derecho);
            }
        }

        public void RecorrerPostOrden()
        {
            RecorrerPostOrdenRecursivo(_raiz);
        }

        private void RecorrerPostOrdenRecursivo(Nodo nodoActual)
        {
            if (nodoActual != null)
            {
                RecorrerPostOrdenRecursivo(nodoActual.Izquierdo);
                RecorrerPostOrdenRecursivo(nodoActual.Derecho);
                Console.WriteLine(nodoActual.Valor);
            }
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            ArbolBinario arbol = new ArbolBinario();

            arbol.Insertar(50);
            arbol.Insertar(30);
            arbol.Insertar(70);
            arbol.Insertar(20);
            arbol.Insertar(40);
            arbol.Insertar(60);
            arbol.Insertar(80);

            Console.WriteLine("Recorrido preorden:");
            arbol.RecorrerPreOrden();

            Console.WriteLine("Recorrido inorden:");
            arbol.RecorrerInOrden();

            Console.WriteLine("Recorrido postorden:");
            arbol.RecorrerPostOrden();

            Console.WriteLine("Buscar el valor 40:");
            Console.WriteLine(arbol.Buscar(40));

            Console.WriteLine("Eliminar el valor 40:");
            arbol.Eliminar(40);

            Console.WriteLine("Recorrido inorden despues de eliminar el valor 40:");
            arbol.RecorrerInOrden();

            Console.ReadKey();
        }
    }
}
```

Este código implementa un árbol binario en C#. Un árbol binario es una estructura de datos que almacena datos en nodos que tienen un valor y dos punteros, uno al nodo izquierdo y otro al nodo derecho.

El código primero define la clase `Nodo` que representa un nodo del árbol. La clase tiene tres propiedades: `Valor`, `Izquierdo` y `Derecho`. El valor es el valor del nodo, y los punteros izquierdo y derecho apuntan a los nodos izquierdo y derecho del nodo actual.

El código también define la clase `ArbolBinario` que representa un árbol binario. La clase tiene una propiedad `_raiz` que apunta al nodo raíz del árbol.

El constructor de la clase `ArbolBinario` inicializa la propiedad `_raiz` a `null`.

El método `Insertar` inserta un nodo en el árbol. El método primero comprueba si la raíz es `null`. Si es así, el nuevo nodo se asigna a la raíz. Si la raíz no es `null`, el método llama al método `InsertarRecursivo` para insertar el nodo en el árbol.

El método `InsertarRecursivo` inserta un nodo en el árbol de forma recursiva. El método primero comprueba si el valor del nuevo nodo es menor que el valor del nodo actual. Si es así, el método llama al método `InsertarRecursivo` en el nodo izquierdo del nodo actual. Si el valor del nuevo nodo no es menor que el valor del nodo actual, el método llama al método `InsertarRecursivo` en el nodo derecho del nodo actual.

El método `Buscar` busca un nodo en el árbol. El método primero comprueba si la raíz es `null`. Si es así, el método devuelve `false`. Si la raíz no es `null`, el método llama al método `BuscarRecursivo` para buscar el nodo en el árbol.