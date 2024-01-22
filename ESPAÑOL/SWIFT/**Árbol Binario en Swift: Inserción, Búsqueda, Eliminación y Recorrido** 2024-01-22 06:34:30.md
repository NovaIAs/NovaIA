```swift
import Foundation

// Definición de la clase nodo para el árbol binario.
class Nodo {
    var valor: Int
    var hijoIzquierdo: Nodo?
    var hijoDerecho: Nodo?

    init(valor: Int) {
        self.valor = valor
        self.hijoIzquierdo = nil
        self.hijoDerecho = nil
    }
}

// Definición de la clase árbol binario.
class ArbolBinario {
    var raíz: Nodo?

    func insertar(valor: Int) {
        if let raízActual = raíz {
            insertarRecursivo(nodoActual: raízActual, valor: valor)
        } else {
            raíz = Nodo(valor: valor)
        }
    }

    private func insertarRecursivo(nodoActual: Nodo, valor: Int) {
        if valor < nodoActual.valor {
            if let hijoIzquierdoActual = nodoActual.hijoIzquierdo {
                insertarRecursivo(nodoActual: hijoIzquierdoActual, valor: valor)
            } else {
                nodoActual.hijoIzquierdo = Nodo(valor: valor)
            }
        } else {
            if let hijoDerechoActual = nodoActual.hijoDerecho {
                insertarRecursivo(nodoActual: hijoDerechoActual, valor: valor)
            } else {
                nodoActual.hijoDerecho = Nodo(valor: valor)
            }
        }
    }

    func buscar(valor: Int) -> Bool {
        if let raízActual = raíz {
            return buscarRecursivo(nodoActual: raízActual, valor: valor)
        } else {
            return false
        }
    }

    private func buscarRecursivo(nodoActual: Nodo, valor: Int) -> Bool {
        if nodoActual.valor == valor {
            return true
        } else if valor < nodoActual.valor {
            if let hijoIzquierdoActual = nodoActual.hijoIzquierdo {
                return buscarRecursivo(nodoActual: hijoIzquierdoActual, valor: valor)
            } else {
                return false
            }
        } else {
            if let hijoDerechoActual = nodoActual.hijoDerecho {
                return buscarRecursivo(nodoActual: hijoDerechoActual, valor: valor)
            } else {
                return false
            }
        }
    }

    func eliminar(valor: Int) {
        if let raízActual = raíz {
            eliminarRecursivo(nodoActual: raízActual, valor: valor, nodoPadre: nil)
        }
    }

    private func eliminarRecursivo(nodoActual: Nodo, valor: Int, nodoPadre: Nodo? = nil) {
        if nodoActual.valor == valor {
            if nodoActual.hijoIzquierdo == nil && nodoActual.hijoDerecho == nil {
                // Nodo hoja.
                if nodoPadre?.hijoIzquierdo === nodoActual {
                    nodoPadre?.hijoIzquierdo = nil
                } else {
                    nodoPadre?.hijoDerecho = nil
                }
            } else if nodoActual.hijoIzquierdo != nil && nodoActual.hijoDerecho == nil {
                // Solo hijo izquierdo.
                if nodoPadre?.hijoIzquierdo === nodoActual {
                    nodoPadre?.hijoIzquierdo = nodoActual.hijoIzquierdo
                } else {
                    nodoPadre?.hijoDerecho = nodoActual.hijoIzquierdo
                }
            } else if nodoActual.hijoIzquierdo == nil && nodoActual.hijoDerecho != nil {
                // Solo hijo derecho.
                if nodoPadre?.hijoIzquierdo === nodoActual {
                    nodoPadre?.hijoIzquierdo = nodoActual.hijoDerecho
                } else {
                    nodoPadre?.hijoDerecho = nodoActual.hijoDerecho
                }
            } else {
                // Dos hijos.
                // Encontrar el nodo más a la derecha en el subárbol izquierdo.
                var nodoCandidato = nodoActual.hijoIzquierdo
                while nodoCandidato?.hijoDerecho != nil {
                    nodoCandidato = nodoCandidato?.hijoDerecho
                }

                // Reemplazar el nodo actual con el nodo candidato.
                nodoActual.valor = nodoCandidato!.valor

                // Eliminar el nodo candidato.
                eliminarRecursivo(nodoActual: nodoCandidato!, valor: nodoCandidato!.valor, nodoPadre: nodoActual)
            }
        } else if valor < nodoActual.valor {
            if let hijoIzquierdoActual = nodoActual.hijoIzquierdo {
                eliminarRecursivo(nodoActual: hijoIzquierdoActual, valor: valor, nodoPadre: nodoActual)
            }
        } else {
            if let hijoDerechoActual = nodoActual.hijoDerecho {
                eliminarRecursivo(nodoActual: hijoDerechoActual, valor: valor, nodoPadre: nodoActual)
            }
        }
    }

    func recorridoEnOrden() -> [Int] {
        var valores: [Int] = []
        recorridoEnOrdenRecursivo(nodoActual: raíz!, valores: &valores)
        return valores
    }

    private func recorridoEnOrdenRecursivo(nodoActual: Nodo, valores: inout [Int]) {
        if let hijoIzquierdoActual = nodoActual.hijoIzquierdo {
            recorridoEnOrdenRecursivo(nodoActual: hijoIzquierdoActual, valores: &valores)
        }

        valores.append(nodoActual.valor)

        if let hijoDerechoActual = nodoActual.hijoDerecho {
            recorridoEnOrdenRecursivo(nodoActual: hijoDerechoActual, valores: &valores)
        }
    }
}

// Ejemplo de uso
let árbol = ArbolBinario()
árbol.insertar(valor: 10)
árbol.insertar(valor: 5)
árbol.insertar(valor: 15)
árbol.insertar(valor: 2)
árbol.insertar(valor: 7)
árbol.insertar(valor: 12)
árbol.insertar(valor: 20)

print(árbol.recorridoEnOrden()) // [2, 5, 7, 10, 12, 15, 20]

if árbol.buscar(valor: 15) {
    print("El valor 15 está en el árbol.")
} else {
    print("El valor 15 no está en el árbol.")
}

árbol.eliminar(valor: 10)
print(árbol.recorridoEnOrden()) // [2, 5, 7, 12, 15, 20]
```

Explicación:

* La clase `Nodo` define los nodos individuales del árbol binario. Cada nodo tiene un valor entero, un puntero al nodo hijo izquierdo y un puntero al nodo hijo derecho.
* La clase `ArbolBinario` define el árbol binario en sí. Tiene un puntero al nodo raíz, que es el nodo superior del árbol.
* El método `insertar` inserta un nuevo nodo con el valor dado en el árbol binario. Si el árbol está vacío, el nuevo nodo se convierte en la raíz. De lo contrario, el método llama al método recursivo `insertarRecursivo` para encontrar la posición correcta para el nuevo nodo.
* El método `buscar` busca un nodo con el valor dado en el árbol binario. Si el árbol está vacío, el método devuelve falso. De lo contrario, el método llama al método recursivo `buscarRecursivo` para buscar el nodo.
* El método `eliminar` elimina un nodo con el valor dado del árbol binario. Si el árbol está vacío, el método no hace nada. De lo contrario, el método llama al método recursivo `eliminarRecursivo` para eliminar el nodo.
* El método `recorridoEnOrden` realiza un recorrido en orden del árbol binario. Un recorrido en orden visita los nodos del árbol en el siguiente orden: primero todos los nodos del subárbol izquierdo, luego el nodo actual y finalmente todos los nodos del subárbol derecho. El método llama al método recursivo `recorridoEnOrdenRecursivo` para realizar el recorrido.

El ejemplo de uso crea un árbol binario, inserta algunos valores en el árbol, busca un valor en el árbol, elimina un valor del árbol y realiza un recorrido en orden del árbol.