```swift
import Foundation

// Definir una estructura de datos para representar un nodo de un árbol binario de búsqueda

struct Nodo<T: Comparable> {

    var valor: T
    var hijoIzquierdo: Nodo<T>?
    var hijoDerecho: Nodo<T>?

    // Inicializador
    init(valor: T) {
        self.valor = valor
    }

    // Insertar un nuevo nodo en el árbol
    mutating func insertar(nuevoValor: T) {
        // Si el nuevo valor es menor que el valor actual, ir al subárbol izquierdo
        if nuevoValor < self.valor {
            if let hijoIzquierdo = self.hijoIzquierdo {
                // Si el subárbol izquierdo no está vacío, llamarse recursivamente para continuar la inserción
                hijoIzquierdo.insertar(nuevoValor: nuevoValor)
            } else {
                // Si el subárbol izquierdo está vacío, crear un nuevo nodo y asignárselo a la propiedad hijoIzquierdo
                self.hijoIzquierdo = Nodo(valor: nuevoValor)
            }
        }

        // Si el nuevo valor es mayor que el valor actual, ir al subárbol derecho
        else {
            if let hijoDerecho = self.hijoDerecho {
                // Si el subárbol derecho no está vacío, llamarse recursivamente para continuar la inserción
                hijoDerecho.insertar(nuevoValor: nuevoValor)
            } else {
                // Si el subárbol derecho está vacío, crear un nuevo nodo y asignárselo a la propiedad hijoDerecho
                self.hijoDerecho = Nodo(valor: nuevoValor)
            }
        }
    }

    // Buscar un valor en el árbol
    func buscar(valor: T) -> Bool {
        // Si el valor actual es igual al valor buscado, devolver verdadero
        if self.valor == valor {
            return true
        }

        // Si el valor buscado es menor que el valor actual, ir al subárbol izquierdo
        if valor < self.valor {
            if let hijoIzquierdo = self.hijoIzquierdo {
                // Si el subárbol izquierdo no está vacío, llamarse recursivamente para continuar la búsqueda
                return hijoIzquierdo.buscar(valor: valor)
            } else {
                // Si el subárbol izquierdo está vacío, el valor buscado no está en el árbol
                return false
            }
        }

        // Si el valor buscado es mayor que el valor actual, ir al subárbol derecho
        else {
            if let hijoDerecho = self.hijoDerecho {
                // Si el subárbol derecho no está vacío, llamarse recursivamente para continuar la búsqueda
                return hijoDerecho.buscar(valor: valor)
            } else {
                // Si el subárbol derecho está vacío, el valor buscado no está en el árbol
                return false
            }
        }
    }

    // Eliminar un valor del árbol
    mutating func eliminar(valor: T) {
        // Si el valor actual es igual al valor a eliminar, eliminar el nodo actual y reorganizar el árbol

        if self.valor == valor {
            // Si el nodo actual no tiene hijos, simplemente eliminarlo
            if self.hijoIzquierdo == nil && self.hijoDerecho == nil {
                self = Nodo<T>(valor: valor)
            }

            // Si el nodo actual tiene un solo hijo, reemplazar el nodo actual por su hijo
            else if let hijo = self.hijoIzquierdo {
                self = hijo
            } else if let hijo = self.hijoDerecho {
                self = hijo
            }
        }

        // Si el valor a eliminar es menor que el valor actual, ir al subárbol izquierdo y continuar la eliminación
        else if valor < self.valor {
            if let hijoIzquierdo = self.hijoIzquierdo {
                hijoIzquierdo.eliminar(valor: valor)
            }
        }

        // Si el valor a eliminar es mayor que el valor actual, ir al subárbol derecho y continuar la eliminación
        else {
            if let hijoDerecho = self.hijoDerecho {
                hijoDerecho.eliminar(valor: valor)
            }
        }
    }

    // Mostrar el árbol en orden (izquierda, raíz, derecha)
    func mostrarEnOrden() {
        // Mostrar el subárbol izquierdo en orden
        if let hijoIzquierdo = self.hijoIzquierdo {
            hijoIzquierdo.mostrarEnOrden()
        }

        // Mostrar el valor actual
        print(self.valor)

        // Mostrar el subárbol derecho en orden
        if let hijoDerecho = self.hijoDerecho {
            hijoDerecho.mostrarEnOrden()
        }
    }

    // Mostrar el árbol en preorden (raíz, izquierda, derecha)
    func mostrarEnPreOrden() {
        // Mostrar el valor actual
        print(self.valor)

        // Mostrar el subárbol izquierdo en preorden
        if let hijoIzquierdo = self.hijoIzquierdo {
            hijoIzquierdo.mostrarEnPreOrden()
        }

        // Mostrar el subárbol derecho en preorden
        if let hijoDerecho = self.hijoDerecho {
            hijoDerecho.mostrarEnPreOrden()
        }
    }

    // Mostrar el árbol en postorden (izquierda, derecha, raíz)
    func mostrarEnPostOrden() {
        // Mostrar el subárbol izquierdo en postorden
        if let hijoIzquierdo = self.hijoIzquierdo {
            hijoIzquierdo.mostrarEnPostOrden()
        }

        // Mostrar el subárbol derecho en postorden
        if let hijoDerecho = self.hijoDerecho {
            hijoDerecho.mostrarEnPostOrden()
        }

        // Mostrar el valor actual
        print(self.valor)
    }
}

// Crear un árbol binario de búsqueda y agregar algunos valores

var arbol = Nodo<Int>(valor: 50)
arbol.insertar(nuevoValor: 25)
arbol.insertar(nuevoValor: 75)
arbol.insertar(nuevoValor: 12)
arbol.insertar(nuevoValor: 37)
arbol.insertar(nuevoValor: 87)
arbol.insertar(nuevoValor: 62)
arbol.insertar(nuevoValor: 93)

// Mostrar el árbol en orden
print("Mostrar el árbol en orden:")
arbol.mostrarEnOrden()

// Mostrar el árbol en preorden
print("\nMostrar el árbol en preorden:")
arbol.mostrarEnPreOrden()

// Mostrar el árbol en postorden
print("\nMostrar el árbol en postorden:")
arbol.mostrarEnPostOrden()

// Buscar un valor en el árbol
if arbol.buscar(valor: 37) {
    print("\nEl valor 37 está en el árbol")
} else {
    print("\nEl valor 37 no está en el árbol")
}

// Eliminar un valor del árbol
arbol.eliminar(valor: 25)

// Mostrar el árbol en orden después de eliminar el valor 25
print("\nMostrar el árbol en orden después de eliminar el valor 25:")
arbol.mostrarEnOrden()
```

Este código define una estructura de datos para representar un nodo de un árbol binario de búsqueda, y luego crea un árbol binario de búsqueda e inserta algunos valores en él. A continuación, muestra el árbol en orden, en preorden y en postorden. También busca un valor en el árbol y lo elimina. Por último, vuelve a mostrar el árbol en orden para mostrar el efecto de la eliminación.