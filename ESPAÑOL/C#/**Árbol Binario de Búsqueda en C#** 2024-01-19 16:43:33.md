```c#
// Clase para representar un nodo de un árbol binario de búsqueda
public class NodoArbolBinario<T> where T : IComparable<T>
{
    public T Valor { get; set; }
    public NodoArbolBinario<T> Izquierdo { get; set; }
    public NodoArbolBinario<T> Derecho { get; set; }

    // Constructor para inicializar el nodo
    public NodoArbolBinario(T valor)
    {
        Valor = valor;
        Izquierdo = null;
        Derecho = null;
    }
}

// Clase para representar un árbol binario de búsqueda
public class ArbolBinarioBusqueda<T> where T : IComparable<T>
{
    public NodoArbolBinario<T> Raiz { get; set; }

    // Constructor para inicializar el árbol
    public ArbolBinarioBusqueda()
    {
        Raiz = null;
    }

    // Método para insertar un nuevo valor en el árbol
    public void Insertar(T valor)
    {
        if (Raiz == null)
        {
            Raiz = new NodoArbolBinario<T>(valor);
        }
        else
        {
            InsertarRecursivo(valor, Raiz);
        }
    }

    // Método recursivo para insertar un nuevo valor en el árbol
    private void InsertarRecursivo(T valor, NodoArbolBinario<T> nodoActual)
    {
        if (valor.CompareTo(nodoActual.Valor) < 0)
        {
            if (nodoActual.Izquierdo == null)
            {
                nodoActual.Izquierdo = new NodoArbolBinario<T>(valor);
            }
            else
            {
                InsertarRecursivo(valor, nodoActual.Izquierdo);
            }
        }
        else if (valor.CompareTo(nodoActual.Valor) > 0)
        {
            if (nodoActual.Derecho == null)
            {
                nodoActual.Derecho = new NodoArbolBinario<T>(valor);
            }
            else
            {
                InsertarRecursivo(valor, nodoActual.Derecho);
            }
        }
    }

    // Método para buscar un valor en el árbol
    public bool Buscar(T valor)
    {
        if (Raiz == null)
        {
            return false;
        }
        else
        {
            return BuscarRecursivo(valor, Raiz);
        }
    }

    // Método recursivo para buscar un valor en el árbol
    private bool BuscarRecursivo(T valor, NodoArbolBinario<T> nodoActual)
    {
        if (valor.CompareTo(nodoActual.Valor) < 0)
        {
            if (nodoActual.Izquierdo == null)
            {
                return false;
            }
            else
            {
                return BuscarRecursivo(valor, nodoActual.Izquierdo);
            }
        }
        else if (valor.CompareTo(nodoActual.Valor) > 0)
        {
            if (nodoActual.Derecho == null)
            {
                return false;
            }
            else
            {
                return BuscarRecursivo(valor, nodoActual.Derecho);
            }
        }
        else
        {
            return true;
        }
    }

    // Método para eliminar un valor del árbol
    public void Eliminar(T valor)
    {
        if (Raiz == null)
        {
            return;
        }
        else
        {
            EliminarRecursivo(valor, Raiz, null);
        }
    }

    // Método recursivo para eliminar un valor del árbol
    private void EliminarRecursivo(T valor, NodoArbolBinario<T> nodoActual, NodoArbolBinario<T> padre)
    {
        if (valor.CompareTo(nodoActual.Valor) < 0)
        {
            if (nodoActual.Izquierdo == null)
            {
                return;
            }
            else
            {
                EliminarRecursivo(valor, nodoActual.Izquierdo, nodoActual);
            }
        }
        else if (valor.CompareTo(nodoActual.Valor) > 0)
        {
            if (nodoActual.Derecho == null)
            {
                return;
            }
            else
            {
                EliminarRecursivo(valor, nodoActual.Derecho, nodoActual);
            }
        }
        else
        {
            if (nodoActual.Izquierdo == null && nodoActual.Derecho == null)
            {
                if (padre == null)
                {
                    Raiz = null;
                }
                else if (padre