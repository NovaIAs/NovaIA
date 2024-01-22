```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ComplejoCodigoCSharp
{
    // Esta clase define una interfaz para los objetos que pueden ser comparados.
    public interface IComparable<T>
    {
        // Este método compara el objeto actual con otro objeto y devuelve un entero que indica si el objeto actual es menor que, igual a o mayor que el otro objeto.
        int CompareTo(T other);
    }

    // Esta clase define una clase genérica que implementa la interfaz IComparable<T>.
    public class Comparador<T> : IComparable<T> where T : IComparable<T>
    {
        // Este método compara el objeto actual con otro objeto y devuelve un entero que indica si el objeto actual es menor que, igual a o mayor que el otro objeto.
        public int CompareTo(T other)
        {
            return this.CompareTo(other);
        }
    }

    // Esta clase define una clase que representa una fracción.
    public class Fraccion : IComparable<Fraccion>
    {
        // Los atributos privados de la clase.
        private int _numerador;
        private int _denominador;

        // El constructor de la clase.
        public Fraccion(int numerador, int denominador)
        {
            // Comprobamos que el denominador no sea cero.
            if (denominador == 0)
            {
                throw new ArgumentException("El denominador no puede ser cero.");
            }

            // Asignamos los valores de los atributos.
            this._numerador = numerador;
            this._denominador = denominador;
        }

        // El método ToString() de la clase.
        public override string ToString()
        {
            // Devolvemos la fracción en formato string.
            return $"{this._numerador}/{this._denominador}";
        }

        // El método CompareTo() de la clase.
        public int CompareTo(Fraccion other)
        {
            // Comparamos los numeradores de las fracciones.
            int comparacionNumeradores = this._numerador.CompareTo(other._numerador);

            // Si los numeradores son iguales, comparamos los denominadores.
            if (comparacionNumeradores == 0)
            {
                return this._denominador.CompareTo(other._denominador);
            }

            // Devolvemos el resultado de la comparación de los numeradores.
            return comparacionNumeradores;
        }
    }

    // Esta clase define una clase que representa un árbol binario de búsqueda.
    public class ArbolBinarioDeBusqueda<T> where T : IComparable<T>
    {
        // Los atributos privados de la clase.
        private Nodo<T> _raiz;

        // El constructor de la clase.
        public ArbolBinarioDeBusqueda()
        {
            // La raíz del árbol es nula.
            this._raiz = null;
        }

        // El método Insertar() de la clase.
        public void Insertar(T valor)
        {
            // Creamos un nuevo nodo con el valor especificado.
            Nodo<T> nuevoNodo = new Nodo<T>(valor);

            // Si la raíz del árbol es nula, asignamos el nuevo nodo como raíz.
            if (this._raiz == null)
            {
                this._raiz = nuevoNodo;
                return;
            }

            // Insertamos el nuevo nodo en el árbol.
            this._Insertar(nuevoNodo, this._raiz);
        }

        // El método privado _Insertar() de la clase.
        private void _Insertar(Nodo<T> nuevoNodo, Nodo<T> nodoActual)
        {
            // Comparamos el valor del nuevo nodo con el valor del nodo actual.
            int comparacion = nuevoNodo.Valor.CompareTo(nodoActual.Valor);

            // Si el valor del nuevo nodo es menor que el valor del nodo actual, insertamos el nuevo nodo en el hijo izquierdo del nodo actual.
            if (comparacion < 0)
            {
                // Si el hijo izquierdo del nodo actual es nulo, asignamos el nuevo nodo como hijo izquierdo del nodo actual.
                if (nodoActual.HijoIzquierdo == null)
                {
                    nodoActual.HijoIzquier