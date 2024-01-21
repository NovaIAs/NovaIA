```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CodigoComplejoEnCSharp
{
    class Program
    {
        // Clase para representar un nodo en un árbol binario de búsqueda.
        class Nodo
        {
            public int valor;
            public Nodo hijoIzquierdo;
            public Nodo hijoDerecho;

            public Nodo(int valor)
            {
                this.valor = valor;
                this.hijoIzquierdo = null;
                this.hijoDerecho = null;
            }
        }

        // Clase para representar un árbol binario de búsqueda.
        class ArbolBinarioDeBusqueda
        {
            public Nodo raiz;

            // Método para insertar un nuevo valor en el árbol.
            public void Insertar(int valor)
            {
                if (raiz == null)
                {
                    raiz = new Nodo(valor);
                }
                else
                {
                    InsertarRecursivo(raiz, valor);
                }
            }

            // Método recursivo para insertar un nuevo valor en el árbol.
            private void InsertarRecursivo(Nodo nodoActual, int valor)
            {
                if (valor < nodoActual.valor)
                {
                    if (nodoActual.hijoIzquierdo == null)
                    {
                        nodoActual.hijoIzquierdo = new Nodo(valor);
                    }
                    else
                    {
                        InsertarRecursivo(nodoActual.hijoIzquierdo, valor);
                    }
                }
                else
                {
                    if (nodoActual.hijoDerecho == null)
                    {
                        nodoActual.hijoDerecho = new Nodo(valor);
                    }
                    else
                    {
                        InsertarRecursivo(nodoActual.hijoDerecho, valor);
                    }
                }
            }

            // Método para buscar un valor en el árbol.
            public bool Buscar(int valor)
            {
                if (raiz == null)
                {
                    return false;
                }
                else
                {
                    return BuscarRecursivo(raiz, valor);
                }
            }

            // Método recursivo para buscar un valor en el árbol.
            private bool BuscarRecursivo(Nodo nodoActual, int valor)
            {
                if (nodoActual == null)
                {
                    return false;
                }
                else if (nodoActual.valor == valor)
                {
                    return true;
                }
                else if (valor < nodoActual.valor)
                {
                    return BuscarRecursivo(nodoActual.hijoIzquierdo, valor);
                }
                else
                {
                    return BuscarRecursivo(nodoActual.hijoDerecho, valor);
                }
            }

            // Método para eliminar un valor del árbol.
            public void Eliminar(int valor)
            {
                if (raiz == null)
                {
                    return;
                }
                else
                {
                    EliminarRecursivo(raiz, valor);
                }
            }

            // Método recursivo para eliminar un valor del árbol.
            private void EliminarRecursivo(Nodo nodoActual, int valor)
            {
                if (nodoActual == null)
                {
                    return;
                }
                else if (valor < nodoActual.valor)
                {
                    EliminarRecursivo(nodoActual.hijoIzquierdo, valor);
                }
                else if (valor > nodoActual.valor)
                {
                    EliminarRecursivo(nodoActual.hijoDerecho, valor);
                }
                else
                {
                    // Caso 1: Nodo sin hijos
                    if (nodoActual.hijoIzquierdo == null && nodoActual.hijoDerecho == null)
                    {
                        nodoActual = null;
                    }
                    // Caso 2: Nodo con un solo hijo
                    else if (nodoActual.hijoIzquierdo == null)
                    {
                        nodo