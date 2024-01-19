```swift
// Importamos las bibliotecas necesarias
import Foundation
import UIKit

// Esta clase representa a un videojuego
class Videojuego {
    // Atributos del videojuego
    var nombre: String
    var genero: String
    var plataforma: String
    var añoLanzamiento: Int

    // Constructor del videojuego
    init(nombre: String, genero: String, plataforma: String, añoLanzamiento: Int) {
        self.nombre = nombre
        self.genero = genero
        self.plataforma = plataforma
        self.añoLanzamiento = añoLanzamiento
    }

    // Método que devuelve una descripción del videojuego
    func descripcion() -> String {
        return "El videojuego '\(nombre)' es un juego de género \(genero) que fue lanzado en \(plataforma) en el año \(añoLanzamiento)."
    }
}

// Creamos una instancia de la clase Videojuego
let videojuego = Videojuego(nombre: "Super Mario Bros.", genero: "Plataformas", plataforma: "NES", añoLanzamiento: 1985)

// Imprimimos la descripción del videojuego
print(videojuego.descripcion())

// Ejemplo de uso de expresiones regulares
let texto = "El texto tiene números 12345 y palabras"
let expresionRegular = "[0-9]+" // Busca números del 0 al 9
let resultado = texto.range(of: expresionRegular, options: .regularExpression)

// Imprimimos el resultado de la expresión regular
if let resultado = resultado {
    print("Números encontrados: \(texto[resultado])")
} else {
    print("No se encontraron números en el texto.")
}

// Ejemplo de uso de programación funcional

// Esta función devuelve el doble de un número
func doblar(_ numero: Int) -> Int {
    return numero * 2
}

// Esta función aplica una función a una colección de elementos
func aplicarFuncionAColeccion<T>(_ coleccion: [T], _ funcion: (T) -> T) -> [T] {
    var nuevaColeccion: [T] = []
    for elemento in coleccion {
        nuevaColeccion.append(funcion(elemento))
    }
    return nuevaColeccion
}

// Aplicamos la función doblar a una colección de números
let numeros = [1, 2, 3, 4, 5]
let numerosDoblados = aplicarFuncionAColeccion(numeros, doblar)

// Imprimimos los números doblados
print("Números doblados: \(numerosDoblados)")

// Ejemplo de uso de estructuras de datos avanzadas

// Creamos una estructura que representa a un nodo de un árbol binario
struct Nodo<T> {
    var valor: T
    var hijoIzquierdo: Nodo<T>?
    var hijoDerecho: Nodo<T>?
}

// Creamos una clase que representa a un árbol binario
class ArbolBinario<T> {
    var raiz: Nodo<T>?

    // Método que inserta un nodo en el árbol
    func insertar(_ valor: T) {
        if let raiz = raiz {
            // Si la raíz existe, llamamos al método insertar recursivo
            insertarRecursivo(valor, nodo: raiz)
        } else {
            // Si la raíz no existe, creamos un nuevo nodo y lo establecemos como raíz
            raiz = Nodo(valor: valor, hijoIzquierdo: nil, hijoDerecho: nil)
        }
    }

    // Método que inserta un nodo en el árbol de forma recursiva
    private func insertarRecursivo(_ valor: T, nodo: Nodo<T>) {
        if valor < nodo.valor {
            // Si el valor es menor que el valor del nodo, llamamos al método insertar recursivo en el hijo izquierdo
            if let hijoIzquierdo = nodo.hijoIzquierdo {
                insertarRecursivo(valor, nodo: hijoIzquierdo)
            } else {
                // Si el hijo izquierdo no existe, creamos un nuevo nodo y lo establecemos como hijo izquierdo
                nodo.hijoIzquierdo = Nodo(valor: valor, hijoIzquierdo: nil, hijoDerecho: nil)
            }
        } else {
            // Si el valor es mayor o igual que el valor del nodo, llamamos al método insertar recursivo en el hijo derecho
            if let hijoDerecho = nodo.hijoDerecho {
                insertarRecursivo(valor, nodo: hijoDerecho)
            } else {
                // Si el hijo derecho no existe, creamos un nuevo nodo y lo establecemos como hijo derecho
                nodo.hijoDerecho = Nodo(valor: valor, hijoIzquierdo: nil, hijoDerecho: nil)
            }
        }
    }

    // Método que busca un nodo en el árbol
    func buscar(_ valor: T) -> Nodo<T>? {
        if let raiz = raiz {
            // Si la raíz existe, llamamos al método buscar recursivo
            return buscarRecursivo(valor, nodo: raiz)
        } else {
            // Si la raíz no existe, devolvemos nil
            return nil
        }
    }

    // Método que busca un nodo en el árbol de forma recursiva
    private func buscarRecursivo(_ valor: T, nodo: Nodo<T>) -> Nodo<T>? {
        if valor == nodo.valor {
            // Si el valor es igual al valor del nodo, devolvemos el nodo
            return nodo
        } else if valor < nodo.valor {
            // Si el valor es menor que el valor del nodo, llamamos al método buscar recursivo en el hijo izquierdo
            if let hijoIzquierdo = nodo.hijoIzquierdo {
                return buscarRecursivo(valor, nodo: hijoIzquierdo)
            } else {
                // Si el hijo izquierdo no existe, devolvemos nil
                return nil
            }
        } else {
            // Si el valor es mayor que el valor del nodo, llamamos al método buscar recursivo en el hijo derecho
            if let hijoDerecho = nodo.hijoDerecho {
                return buscarRecursivo(valor, nodo: hijoDerecho)
            } else {
                // Si el hijo derecho no existe, devolvemos nil
                return nil
            }
        }
    }

    // Método que elimina un nodo del árbol
    func eliminar(_ valor: T) {
        if let raiz = raiz {
            // Si la raíz existe, llamamos al método eliminar recursivo
            raiz = eliminarRecursivo(valor, nodo: raiz)
        }
    }

    // Método que elimina un nodo del árbol de forma recursiva
    private func eliminarRecursivo(_ valor: T, nodo: Nodo<T>) -> Nodo<T>? {
        if valor == nodo.valor {
            // Si el valor es igual al valor del nodo, eliminamos el nodo
            if nodo.hijoIzquierdo == nil && nodo.hijoDerecho == nil {
                // Si el nodo no tiene hijos, lo eliminamos directamente
                return nil
            } else if nodo.hijoIzquierdo != nil && nodo.hijoDerecho == nil {
                // Si el nodo tiene un solo hijo izquierdo, lo establecemos como nuevo nodo
                return nodo.hijoIzquierdo
            } else