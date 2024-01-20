```d
// !mark Implementación de un Árbol Binario de Búsqueda en D

import std.stdio, std.algorithm, std.container;

class BSTNode!T
{
    T value;
    BSTNode!T left;
    BSTNode!T right;

    // Constructor con valor y nodos izquierdo y derecho por defecto en null
    this(T value) pure nothrow @safe
    {
        this.value = value;
        this.left = null;
        this.right = null;
    }
}

class BST!T
{
    BSTNode!T root;

    // Constructor vacío que crea un árbol binario de búsqueda vacío
    this() pure nothrow @safe
    {
        this.root = null;
    }

    // Inserta un nuevo valor en el árbol binario de búsqueda, manteniendo el orden de los valores
    void insert(T value) pure nothrow @safe
    {
        // Si el árbol está vacío, el nuevo valor se convierte en la raíz
        if (this.root == null)
        {
            this.root = new BSTNode!T(value);
            return;
        }

        // Si el valor del nodo actual es mayor que el valor a insertar, inserta recursivamente en el subárbol izquierdo
        if (value < this.root.value)
        {
            if (this.root.left == null)
            {
                this.root.left = new BSTNode!T(value);
            }
            else
            {
                this.root.left.insert(value);
            }
        }
        // Si el valor del nodo actual es menor o igual que el valor a insertar, inserta recursivamente en el subárbol derecho
        else
        {
            if (this.root.right == null)
            {
                this.root.right = new BSTNode!T(value);
            }
            else
            {
                this.root.right.insert(value);
            }
        }
    }

    // Busca un valor en el árbol binario de búsqueda y devuelve el nodo que lo contiene, o null si no lo encuentra
    BSTNode!T find(T value) pure nothrow @safe
    {
        // Comenzamos la búsqueda desde la raíz
        BSTNode!T current = this.root;

        // Mientras el nodo actual no sea null y el valor a buscar no sea igual al valor del nodo actual, continuamos
        while (current != null && current.value != value)
        {
            // Si el valor a buscar es menor que el valor del nodo actual, avanzamos al subárbol izquierdo
            if (value < current.value)
            {
                current = current.left;
            }
            // Si el valor a buscar es mayor o igual que el valor del nodo actual, avanzamos al subárbol derecho
            else
            {
                current = current.right;
            }
        }

        // Devolvemos el nodo que contiene el valor a buscar, o null si no lo encontramos
        return current;
    }

    // Elimina un valor del árbol binario de búsqueda
    void remove(T value) pure nothrow @safe
    {
        // Comenzamos la búsqueda del nodo que contiene el valor a eliminar desde la raíz
        BSTNode!T current = this.root;
        BSTNode!T parent = null;

        // Seguimos el camino hasta encontrar el nodo que contiene el valor a eliminar
        while (current != null && current.value != value)
        {
            // Actualizamos el nodo padre del nodo actual
            parent = current;

            // Si el valor a eliminar es menor que el valor del nodo actual, avanzamos al subárbol izquierdo
            if (value < current.value)
            {
                current = current.left;
            }
            // Si el valor a eliminar es mayor o igual que el valor del nodo actual, avanzamos al subárbol derecho
            else
            {
                current = current.right;
            }
        }

        // Si no encontramos el nodo que contiene el valor a eliminar, salimos de la función
        if (current == null)
        {
            return;
        }

        // Si el nodo que contiene el valor a eliminar no tiene hijos, simplemente lo eliminamos
        if (current.left == null && current.right == null)
        {
            // Actualizamos el nodo padre del nodo actual para que apunte a null
            if (parent == null)
            {
                this.root = null;
            }
            else
            {
                if (parent.left == current)
                {
                    parent.left = null;
                }
                else
                {
                    parent.right = null;
                }
            }

            // Eliminamos el nodo actual
            current = null;
        }
        // Si el nodo que contiene el valor a eliminar tiene un solo hijo, lo ascendemos
        else if (current.left == null || current.right == null)
        {
            // Si el nodo que contiene el valor a eliminar tiene un solo hijo izquierdo, lo ascendemos
            if (current.left != null)
            {
                // Actualizamos el nodo padre del nodo actual para que apunte al hijo izquierdo del nodo actual
                if (parent == null)
                {
                    this.root = current.left;
                }
                else
                {
                    if (parent.left == current)
                    {
                        parent.left = current.left;
                    }
                    else
                    {
                        parent.right = current.left;
                    }
                }

                // Eliminamos el nodo actual
                current = null;
            }
            // Si el nodo que contiene el valor a eliminar tiene un solo hijo derecho, lo ascendemos
            else
            {
                // Actualizamos el nodo padre del nodo actual para que apunte al hijo derecho del nodo actual
                if (parent == null)
                {
                    this.root = current.right;
                }
                else
                {
                    if (parent.left == current)
                    {
                        parent.left = current.right;
                    }
                    else
                    {
                        parent.right = current.right;
                    }
                }

                // Eliminamos el nodo actual
                current = null;
            }
        }
        // Si el nodo que contiene el valor a eliminar tiene dos hijos, lo reemplazamos por su predecesor
        else
        {
            // Buscamos el predecesor del nodo que contiene el valor a eliminar
            BSTNode!T predecessor = current.left;
            BSTNode!T predecessorParent = current;

            while (predecessor.right != null)
            {
                predecessorParent = predecessor;
                predecessor = predecessor.right;
            }

            // Reemplazamos el valor del nodo que contiene el valor a eliminar por el valor del predecesor
            current.value = predecessor.value;

            // Eliminamos el predecesor
            if (predecessorParent == current)
            {
                predecessorParent.left = predecessor.left;
            }
            else
            {
                predecessorParent.right = predecessor.left;
            }

            predecessor = null;
        }
    }

    // Devuelve una lista con los valores del árbol binario de búsqueda en orden ascendente
    immutable T[] getValues() pure nothrow @safe
    {
        // Creamos una lista para almacenar los valores
        ArrayList!T values = new ArrayList!T();

        // Llamamos a la función recursiva que recorre el árbol binario de búsqueda en orden ascendente y almacena los valores en la lista
        getInOrderValues(this.root, values);

        // Devolvemos la lista con los valores
        return values.dup;
    }

    // Función recursiva que recorre el árbol binario de búsqueda en orden ascendente y almacena los valores en una lista
    immutable void getInOrderValues(BSTNode!T node, ArrayList!T values) pure nothrow @nogc @safe
    {
        // Si el nodo actual es nulo, salimos de la función
        if (node == null)
        {
            return;
        }

        // Llamamos a la función recursiva para recorrer el subárbol izquierdo del nodo actual
        getInOrderValues(node.left, values);

        // Almacenamos el valor del nodo actual en la lista
        values.push(node.value);

        // Llamamos a la función recursiva para recorrer el subárbol derecho del nodo actual
        getInOrderValues(node.right, values);
    }

    // Devuelve una lista con los valores del árbol binario de búsqueda en orden descendente
    immutable T[] getValuesReversed() pure nothrow @safe
    {
        // Creamos una lista para almacenar los valores
        ArrayList!T values = new ArrayList!T();

        // Llamamos a la función recursiva que recorre el árbol binario de búsqueda en orden descendente y almacena los valores en la lista
        getReversedValues(this.root, values);

        // Devolvemos la lista con los valores
        return values.dup;
    }

    // Función recursiva que recorre el árbol binario de búsqueda en orden descendente y almacena los valores en una lista
    immutable void getReversedValues(BSTNode!T node, ArrayList!T values) pure nothrow @nogc @safe
    {
        // Si el nodo actual es nulo, salimos de la función
        if (node == null)
        {
            return;
        }

        // Llamamos a la función recursiva para recorrer el subárbol derecho del nodo actual
        getReversedValues(node.right, values);

        // Almacenamos el valor del nodo actual en la lista
        values.push(node.value);

        // Llamamos a la función recursiva para recorrer el subárbol izquierdo del nodo actual
        getReversedValues(node.left, values);
    }

    // Devuelve la altura del árbol binario de búsqueda
    immutable size_t getHeight() pure nothrow @safe
    {
        // Llamamos a la función recursiva que calcula la altura del árbol binario de búsqueda
        return getHeight(this.root);
    }

    // Función recursiva que calcula la altura del árbol binario de búsqueda
    immutable size_t getHeight(BSTNode!T node) pure nothrow @nogc @safe
    {
        // Si el nodo actual es nulo, devolvemos 0
        if (node == null)
        {
            return 0;
        }

        // Calculamos la altura del subárbol izquierdo del nodo actual
        size_t leftHeight = getHeight(node.left);

        // Calculamos la altura del subárbol derecho del nodo actual
        size_t rightHeight = getHeight(node.right);

        // Devolvemos la altura máxima entre el subárbol izquierdo y el subárbol derecho, más 1
        return max(leftHeight, rightHeight) + 1;
    }

    // Devuelve el número de nodos del árbol binario de búsqueda
    immutable size_t getCount() pure nothrow @safe
    {
        // Llamamos a la función recursiva que calcula el número de nodos del árbol binario de búsqueda
        return getCount(this.root);
    }

    // Función recursiva que calcula el número de nodos del árbol binario de búsqueda
    immutable size_t getCount(BSTNode!T node) pure nothrow @nogc @safe
    {
        // Si el nodo actual es nulo, devolvemos 0
        if (node == null)
        {
            return 0;
        }

        // Calculamos el número de nodos del subárbol izquierdo del nodo actual
        size_t leftCount = getCount(node.left);

        // Calculamos el número de nodos del subárbol derecho del nodo actual
        size_t rightCount = getCount(node.right);

        // Devolvemos la suma del número de nodos del subárbol izquierdo y el subárbol derecho, más 1
        return leftCount + rightCount + 1;
    }

    // Devuelve el valor máximo del árbol binario de búsqueda
    immutable T getMax() pure nothrow @safe
    {
        // Llamamos a la función recursiva que busca el valor máximo del árbol binario de búsqueda
        return getMax(this.root);
    }

    // Función recursiva que busca el valor máximo del árbol binario de búsqueda
    immutable T getMax(BSTNode!