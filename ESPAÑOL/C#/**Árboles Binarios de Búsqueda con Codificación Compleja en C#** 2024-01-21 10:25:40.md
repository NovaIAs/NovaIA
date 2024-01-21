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
        static void Main(string[] args)
        {
            // 1. Definir una clase genérica para representar un árbol binario de búsqueda.

            public class Nodo<T>
            {
                public T Valor { get; set; }
                public Nodo<T> HijoIzquierdo { get; set; }
                public Nodo<T> HijoDerecho { get; set; }

                public Nodo(T valor)
                {
                    Valor = valor;
                }
            }

            public class ArbolBinario<T> where T : IComparable
            {
                private Nodo<T> _raiz;

                public ArbolBinario()
                {
                    _raiz = null;
                }

                public void Insertar(T valor)
                {
                    if (_raiz == null)
                    {
                        _raiz = new Nodo<T>(valor);
                    }
                    else
                    {
                        InsertarRecursivo(valor, _raiz);
                    }
                }

                private void InsertarRecursivo(T valor, Nodo<T> nodoActual)
                {
                    if (valor.CompareTo(nodoActual.Valor) < 0)
                    {
                        if (nodoActual.HijoIzquierdo == null)
                        {
                            nodoActual.HijoIzquierdo = new Nodo<T>(valor);
                        }
                        else
                        {
                            InsertarRecursivo(valor, nodoActual.HijoIzquierdo);
                        }
                    }
                    else
                    {
                        if (nodoActual.HijoDerecho == null)
                        {
                            nodoActual.HijoDerecho = new Nodo<T>(valor);
                        }
                        else
                        {
                            InsertarRecursivo(valor, nodoActual.HijoDerecho);
                        }
                    }
                }

                public bool Buscar(T valor)
                {
                    if (_raiz == null)
                    {
                        return false;
                    }
                    else
                    {
                        return BuscarRecursivo(valor, _raiz);
                    }
                }

                private bool BuscarRecursivo(T valor, Nodo<T> nodoActual)
                {
                    if (valor.CompareTo(nodoActual.Valor) == 0)
                    {
                        return true;
                    }
                    else if (valor.CompareTo(nodoActual.Valor) < 0)
                    {
                        if (nodoActual.HijoIzquierdo == null)
                        {
                            return false;
                        }
                        else
                        {
                            return BuscarRecursivo(valor, nodoActual.HijoIzquierdo);
                        }
                    }
                    else
                    {
                        if (nodoActual.HijoDerecho == null)
                        {
                            return false;
                        }
                        else
                        {
                            return BuscarRecursivo(valor, nodoActual.HijoDerecho);
                        }
                    }
                }

                public void Eliminar(T valor)
                {
                    if (_raiz == null)
                    {
                        return;
                    }
                    else
                    {
                        EliminarRecursivo(valor, _raiz, null);
                    }
                }

                private void EliminarRecursivo(T valor, Nodo<T> nodoActual, Nodo<T> nodoPadre)
                {
                    if (valor.CompareTo(nodoActual.Valor) == 0)
                    {
                        // Caso 1: Nodo hoja
                        if (nodoActual.HijoIzquierdo == null && nodoActual.HijoDerecho == null)
                        {
                            if (nodoPadre == null)
                            {
                                _raiz = null;
                            }
                            else if (nodoPadre.HijoIzquierdo == nodoActual)
                            {
                                nodoPadre.HijoIzquierdo = null;
                            }
                            else
                            {
                                nodoPadre.HijoDerecho = null;
                            }
                        }
                        // Caso 2: Nodo con un solo hijo
                        else if (nodoActual.HijoIzquierdo != null && nodoActual.HijoDerecho == null)
                        {
                            if (nodoPadre