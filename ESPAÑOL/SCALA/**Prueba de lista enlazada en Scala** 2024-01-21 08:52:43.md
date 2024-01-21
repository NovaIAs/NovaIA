```scala
// Prueba de clase Node
class Node[T](var dato: T, var siguiente: Node[T]) {
    def this(dato: T) = this(dato, null)
}

// Prueba de clase Lista
class Lista[T] {

    // Cabecera de la lista
    var cabecera: Node[T] = null

    // Método para añadir un elemento al final de la lista
    def añadir(dato: T): Unit = {

        // Crear un nuevo nodo que se añadirá
        val nodo = new Node[T](dato)

        // Si la lista está vacía, el nuevo nodo se convierte en la cabecera
        if (cabecera == null) {
            cabecera = nodo
        } else {

            // Si la lista no está vacía, se recorre hasta encontrar el último nodo y luego se añade el nuevo nodo
            var actual = cabecera
            while (actual.siguiente != null) {
                actual = actual.siguiente
            }
            actual.siguiente = nodo
        }
    }

    // Método para imprimir la lista
    def imprimir(): Unit = {

        // Crear una cadena para almacenar los datos de la lista
        var cadena = "Lista: "

        // Recorrer la lista y añadir los datos de cada nodo a la cadena
        var actual = cabecera
        while (actual != null) {
            cadena += actual.dato + " "
            actual = actual.siguiente
        }

        // Imprimir la cadena
        println(cadena)
    }

    // Método para eliminar un elemento de la lista
    def eliminar(dato: T): Unit = {

        // Si la lista está vacía, no se puede eliminar ningún elemento
        if (cabecera == null) {
            println("La lista está vacía.")
        } else {

            // Si el elemento a eliminar es la cabecera, se elimina la cabecera y se asigna el siguiente nodo
            // como nueva cabecera
            if (cabecera.dato == dato) {
                cabecera = cabecera.siguiente
            } else {

                // Si el elemento a eliminar no es la cabecera, se recorre la lista hasta encontrarlo
                // y luego se elimina
                var anterior = cabecera
                var actual = cabecera.siguiente
                while (actual != null && actual.dato != dato) {
                    anterior = actual
                    actual = actual.siguiente
                }

                // Si el elemento a eliminar fue encontrado, se elimina
                if (actual != null) {
                    anterior.siguiente = actual.siguiente
                } else {
                    println("El elemento no se encuentra en la lista.")
                }
            }
        }
    }

    // Método para buscar un elemento en la lista
    def buscar(dato: T): Boolean = {

        // Crear una variable para almacenar si el elemento fue encontrado
        var encontrado = false

        // Recorrer la lista y buscar el elemento
        var actual = cabecera
        while (actual != null && !encontrado) {
            if (actual.dato == dato) {
                encontrado = true
            }
            actual = actual.siguiente
        }

        // Devolver si el elemento fue encontrado
        encontrado
    }
}

// Crear una lista de enteros
val lista = new Lista[Int]

// Añadir algunos elementos a la lista
lista.añadir(1)
lista.añadir(2)
lista.añadir(3)
lista.añadir(4)
lista.añadir(5)

// Imprimir la lista
lista.imprimir()

// Eliminar un elemento de la lista
lista.eliminar(3)

// Imprimir la lista
lista.imprimir()

// Buscar un elemento en la lista
val encontrado = lista.buscar(4)

// Imprimir si el elemento fue encontrado o no
if (encontrado) {
    println("El elemento 4 fue encontrado en la lista.")
} else {
    println("El elemento 4 no fue encontrado en la lista.")
}
```

Explicación del código:

1. **Clase Nodo:** Esta clase representa un nodo de la lista enlazada. Cada nodo tiene un dato y un puntero al siguiente nodo.
2. **Clase Lista:** Esta clase representa una lista enlazada. Tiene una cabecera, que es el primer nodo de la lista, y una serie de métodos para añadir, eliminar y buscar elementos de la lista.
3. **Método añadir:** Este método añade un nuevo elemento al final de la lista. Si la lista está vacía, el nuevo elemento se convierte en la cabecera. Si la lista no está vacía, se recorre hasta encontrar el último nodo y luego se añade el nuevo nodo.
4. **Método imprimir:** Este método imprime los datos de todos los nodos de la lista.
5. **Método eliminar:** Este método elimina un elemento de la lista. Si el elemento a eliminar es la cabecera, se elimina la cabecera y se asigna el siguiente nodo como nueva cabecera. Si el elemento a eliminar no es la cabecera, se recorre la lista hasta encontrarlo y luego se elimina.
6. **Método buscar:** Este método busca un elemento en la lista. Si el elemento se encuentra en la lista, devuelve verdadero. Si no se encuentra, devuelve falso.
7. **Crear una lista:** Se crea una lista de enteros y se añaden algunos elementos a la lista.
8. **Imprimir la lista:** Se imprime la lista.
9. **Eliminar un elemento:** Se elimina el elemento 3 de la lista.
10. **Imprimir la lista:** Se imprime la lista.
11. **Buscar un elemento:** Se busca el elemento 4 en la lista.
12. **Imprimir el resultado:** Se imprime si el elemento 4 fue encontrado o no en la lista.