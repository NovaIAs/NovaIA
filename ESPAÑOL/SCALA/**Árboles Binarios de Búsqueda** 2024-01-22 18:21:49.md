```scala
object ProblemaGrande {

    // Clase que representa a un nodo de un árbol binario de búsqueda.
    class Node[A](value: A) {
        var left: Option[Node[A]] = None
        var right: Option[Node[A]] = None
        def esHoja(): Boolean = left.isEmpty && right.isEmpty
    }

    // Clase que representa al árbol binario de búsqueda.
    class BinarySearchTree[A](root: Option[Node[A]] = None) {

        // Agrega un valor al árbol.
        def agregar(value: A): Unit = {
            _agregar(value, root)
        }

        // Agrega un valor al árbol, de manera recursiva.
        private def _agregar(value: A, current: Option[Node[A]]): Unit = {
            current match {
                case Some(node) =>
                    if (value < node.value) {
                        _agregar(value, node.left)
                    } else {
                        _agregar(value, node.right)
                    }
                case None =>
                    root = Some(new Node(value))
            }
        }

        // Busca un valor en el árbol.
        def buscar(value: A): Option[Node[A]] = {
            _buscar(value, root)
        }

        // Busca un valor en el árbol, de manera recursiva.
        private def _buscar(value: A, current: Option[Node[A]]): Option[Node[A]] = {
            current match {
                case Some(node) =>
                    if (value == node.value) {
                        Some(node)
                    } else if (value < node.value) {
                        _buscar(value, node.left)
                    } else {
                        _buscar(value, node.right)
                    }
                case None =>
                    None
            }
        }

        // Elimina un valor del árbol.
        def eliminar(value: A): Unit = {
            _eliminar(value, root)
        }

        // Elimina un valor del árbol, de manera recursiva.
        private def _eliminar(value: A, current: Option[Node[A]]): Unit = {
            current match {
                case Some(node) =>
                    if (value == node.value) {
                        // Si el nodo no tiene hijos, simplemente lo elimino.
                        if (node.esHoja()) {
                            current = None
                        }
                        // Si el nodo tiene un solo hijo, lo reemplazo por su hijo.
                        else if (node.left.isEmpty) {
                            current = node.right
                        } else if (node.right.isEmpty) {
                            current = node.left
                        }
                        // Si el nodo tiene dos hijos, lo reemplazo por su predecesor.
                        else {
                            val predecessor = _predecessor(node.left)
                            node.value = predecessor.value
                            _eliminar(predecessor.value, node.left)
                        }
                    } else if (value < node.value) {
                        _eliminar(value, node.left)
                    } else {
                        _eliminar(value, node.right)
                    }
                case None =>
            }
        }

        // Busca el predecesor de un nodo.
        private def _predecessor(current: Option[Node[A]]): Node[A] = {
            current.get.right match {
                case Some(node) => _predecessor(node.left)
                case None => current.get
            }
        }

        // Imprime el árbol en forma de árbol.
        def imprimir(): Unit = {
            _imprimir(root)
        }

        // Imprime el árbol en forma de árbol, de manera recursiva.
        private def _imprimir(current: Option[Node[A]]): Unit = {
            current match {
                case Some(node) =>
                    println(node.value)
                    _imprimir(node.left)
                    _imprimir(node.right)
                case None =>
            }
        }
    }

    // Ejemplo de uso del árbol binario de búsqueda.
    def main(args: Array[String]): Unit = {
        val arbol = new BinarySearchTree[Int]()
        arbol.agregar(10)
        arbol.agregar(5)
        arbol.agregar(15)
        arbol.agregar(2)
        arbol.agregar(7)
        arbol.agregar(12)
        arbol.agregar(20)

        println("Árbol binario de búsqueda:")
        arbol.imprimir()

        println("Buscar el valor 15:")
        val nodo15 = arbol.buscar(15)
        if (nodo15.isDefined) {
            println("El valor 15 está en el árbol.")
        } else {
            println("El valor 15 no está en el árbol.")
        }

        println("Eliminar el valor 10:")
        arbol.eliminar(10)

        println("Árbol binario de búsqueda después de eliminar el valor 10:")
        arbol.imprimir()
    }
}
```

Explicación del código:

- La clase `Node` representa a un nodo de un árbol binario de búsqueda. Un nodo tiene un valor y dos enlaces, uno a su hijo izquierdo y otro a su hijo derecho.

- La clase `BinarySearchTree` representa al árbol binario de búsqueda. Un árbol binario de búsqueda es un árbol binario en el que los valores de los nodos se ordenan de manera ascendente.

- El árbol binario de búsqueda tiene los siguientes métodos:

    - `agregar` agrega un valor al árbol.
    - `buscar` busca un valor en el árbol.
    - `eliminar` elimina un valor del árbol.
    - `imprimir` imprime el árbol en forma de árbol.

- Los métodos `agregar`, `buscar`, `eliminar` e `imprimir` utilizan la recursión para recorrer el árbol y realizar las operaciones necesarias.

- El ejemplo de uso del árbol binario de búsqueda crea un árbol binario de búsqueda y luego agrega, busca, elimina e imprime los valores del árbol.

- El código también incluye una explicación de cada uno de los métodos del árbol binario de búsqueda.