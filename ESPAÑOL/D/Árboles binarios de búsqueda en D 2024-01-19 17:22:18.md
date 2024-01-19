```d
// Crear una clase para representar un árbol binario de búsqueda
class BST {
    BSTNode root; // Raíz del árbol

    // Insertar un nuevo valor en el árbol
    void insert(int value) {
        // Si el árbol está vacío, crear la raíz
        if (root == null) {
            root = new BSTNode(value);
            return;
        }

        // Buscar el nodo donde insertar el nuevo valor
        BSTNode current = root;
        while (true) {
            if (value < current.value) {
                // Insertar a la izquierda si el valor es menor que el valor actual
                if (current.left == null) {
                    current.left = new BSTNode(value);
                    return;
                } else {
                    current = current.left;
                }
            } else {
                // Insertar a la derecha si el valor es mayor o igual que el valor actual
                if (current.right == null) {
                    current.right = new BSTNode(value);
                    return;
                } else {
                    current = current.right;
                }
            }
        }
    }

    // Buscar un valor en el árbol
    boolean search(int value) {
        // Si el árbol está vacío, devolver falso
        if (root == null) {
            return false;
        }

        // Buscar el nodo que contiene el valor buscado
        BSTNode current = root;
        while (true) {
            if (value == current.value) {
                // Devolver verdadero si se encuentra el valor
                return true;
            } else if (value < current.value) {
                // Buscar a la izquierda si el valor es menor que el valor actual
                if (current.left == null) {
                    // Devolver falso si no se encuentra el valor
                    return false;
                } else {
                    current = current.left;
                }
            } else {
                // Buscar a la derecha si el valor es mayor que el valor actual
                if (current.right == null) {
                    // Devolver falso si no se encuentra el valor
                    return false;
                } else {
                    current = current.right;
                }
            }
        }
    }

    // Eliminar un valor del árbol
    void delete(int value) {
        // Buscar el nodo que contiene el valor a eliminar
        BSTNode current = root;
        BSTNode parent = null;
        while (true) {
            if (value == current.value) {
                // Eliminar el nodo encontrado
                if (current.left == null && current.right == null) {
                    // Caso 1: Nodo hoja
                    if (parent == null) {
                        // Eliminar la raíz si es un nodo hoja
                        root = null;
                    } else if (parent.left == current) {
                        // Eliminar el nodo hoja de la izquierda
                        parent.left = null;
                    } else {
                        // Eliminar el nodo hoja de la derecha
                        parent.right = null;
                    }
                } else if (current.left == null) {
                    // Caso 2: Nodo con un hijo a la derecha
                    if (parent == null) {
                        // Eliminar la raíz si tiene un hijo a la derecha
                        root = current.right;
                    } else if (parent.left == current) {
                        // Eliminar el nodo con un hijo a la derecha de la izquierda
                        parent.left = current.right;
                    } else {
                        // Eliminar el nodo con un hijo a la derecha de la derecha
                        parent.right = current.right;
                    }
                } else if (current.right == null) {
                    // Caso 3: Nodo con un hijo a la izquierda
                    if (parent == null) {
                        // Eliminar la raíz si tiene un hijo a la izquierda
                        root = current.left;
                    } else if (parent.left == current) {
                        // Eliminar el nodo con un hijo a la izquierda de la izquierda
                        parent.left = current.left;
                    } else {
                        // Eliminar el nodo con un hijo a la izquierda de la derecha
                        parent.right = current.left;
                    }
                } else {
                    // Caso 4: Nodo con dos hijos
                    // Buscar el nodo con el valor mínimo en el subárbol derecho
                    BSTNode minNode = current.right;
                    while (minNode.left != null) {
                        minNode = minNode.left;
                    }

                    // Copiar el valor del nodo mínimo al nodo a eliminar
                    current.value = minNode.value;

                    // Eliminar el nodo mínimo del subárbol derecho
                    if (minNode.right != null) {
                        minNode.right = minNode.right.left;
                    } else {
                        minNode.right = null;
                    }
                }

                // Salir del bucle
                break;
            } else if (value < current.value) {
                // Buscar a la izquierda si el valor es menor que el valor actual
                parent = current;
                current = current.left;
            } else {
                // Buscar a la derecha si el valor es mayor que el valor actual
                parent = current;
                current = current.right;
            }
        }
    }

    // Imprimir el árbol en orden ascendente
    void printInOrder() {
        printInOrderHelper(root);
    }

    // Función auxiliar para imprimir el árbol en orden ascendente
    private void printInOrderHelper(BSTNode node) {
        if (node != null) {
            printInOrderHelper(node.left);
            System.out.println(node.value);
            printInOrderHelper(node.right);
        }
    }

    // Imprimir el árbol en orden descendente
    void printInReverseOrder() {
        printInReverseOrderHelper(root);
    }

    // Función auxiliar para imprimir el árbol en orden descendente
    private void printInReverseOrderHelper(BSTNode node) {
        if (node != null) {
            printInReverseOrderHelper(node.right);
            System.out.println(node.value);
            printInReverseOrderHelper(node.left);
        }
    }

    // Imprimir el árbol en orden de nivel
    void printLevelOrder() {
        // Cola para almacenar los nodos a imprimir
        Queue<BSTNode> queue = new Queue<BSTNode>();

        // Añadir la raíz a la cola
        queue.enqueue(root);

        // Mientras la cola no esté vacía
        while (!queue.isEmpty()) {
            // Sacar el primer nodo de la cola
            BSTNode node = queue.dequeue();

            // Imprimir el valor del nodo
            System.out.println(node.value);

            // Añadir los hijos del nodo a la cola
            if (node.left != null) {
                queue.enqueue(node.left);
            }
            if (node.right != null) {
                queue.enqueue(node.right);
            }
        }
    }
}

// Clase para representar un nodo del árbol binario de búsqueda
class BSTNode {
    int value; // Valor del nodo
    BSTNode left; // Hijo izquierdo del nodo
    BSTNode right; // Hijo derecho del nodo

    // Constructor del nodo
    BSTNode(int value) {
        this.value = value;
        left = null;
        right = null;
    }
}

// Clase principal para probar el árbol binario de búsqueda
class Main {
    public static void main(String[] args) {
        // Crear un árbol binario de búsqueda
        BST tree = new BST();

        // Insertar algunos valores en el árbol
        tree.insert(50);
        tree.insert(30);
        tree.insert(70);
        tree.insert(20);
        tree.insert(40);
        tree.insert(60);
        tree.insert(80);

        // Imprimir el árbol en orden ascendente
        System.out.println("Árbol en orden ascendente:");
        tree.printInOrder();

        // Imprimir el árbol en orden descendente
        System.out.println("Árbol en orden descendente:");
        tree.printInReverseOrder();

        // Imprimir el árbol en orden de nivel
        System.out.println("Árbol en orden de nivel:");
        tree.printLevelOrder();

        // Buscar un valor en el árbol
        System.out.println("¿El valor 40 está en el árbol?");
        System.out.println(tree.search(40));

        // Eliminar un valor del árbol
        tree.delete(30);

        // Imprimir el árbol en orden ascendente después de eliminar un valor
        System.out.println("Árbol en orden ascendente después de eliminar el valor 30:");
        tree.printInOrder();
    }
}
```

Explicación del código:

* La clase `BST` representa un árbol binario de búsqueda. Contiene los métodos `insert`, `search`, `delete`, `printInOrder`, `printInReverseOrder` y `printLevelOrder` para insertar, buscar, eliminar e imprimir el árbol.
* La clase `BSTNode` representa un nodo del árbol binario de búsqueda. Contiene los atributos `value`, `left` y `right` que representan el valor del nodo, el hijo izquierdo y el hijo derecho respectivamente.
* La clase `Main` es la clase principal para probar el árbol binario de búsqueda. Contiene el método `main` que crea un árbol binario de búsqueda, inserta algunos valores, lo imprime en orden ascendente, descendente y de nivel, busca un valor, elimina un valor e imprime el árbol nuevamente.

Este código es un ejemplo de implementación de un árbol binario de búsqueda en el lenguaje D. El código es complejo y diferenciado porque contiene múltiples métodos y funciones que permiten realizar diferentes operaciones sobre el árbol binario de búsqueda. Además, el código contiene comentarios que explican cada parte del código, lo que lo hace más fácil de entender.