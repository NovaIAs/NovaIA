```kotlin
// Clase para representar un nodo de un árbol binario
class Nodo(var dato: Int, var izquierda: Nodo?, var derecha: Nodo?) {
    // Constructor
    constructor() : this(0, null, null)

    // Función para insertar un nodo en el árbol
    fun insertar(nuevoDato: Int) {
        if (nuevoDato < dato) {
            if (izquierda == null) {
                izquierda = Nodo(nuevoDato)
            } else {
                izquierda!!.insertar(nuevoDato)
            }
        } else {
            if (derecha == null) {
                derecha = Nodo(nuevoDato)
            } else {
                derecha!!.insertar(nuevoDato)
            }
        }
    }

    // Función para buscar un nodo en el árbol
    fun buscar(datoBuscado: Int): Nodo? {
        if (datoBuscado == dato) {
            return this
        } else if (datoBuscado < dato) {
            if (izquierda != null) {
                return izquierda!!.buscar(datoBuscado)
            }
        } else {
            if (derecha != null) {
                return derecha!!.buscar(datoBuscado)
            }
        }
        return null
    }

    // Función para eliminar un nodo del árbol
    fun eliminar(datoEliminar: Int) {
        if (datoEliminar == dato) {
            if (izquierda == null && derecha == null) {
                // Nodo hoja
                dato = 0
            } else if (izquierda == null) {
                // Nodo con un hijo derecho
                dato = derecha!!.dato
                derecha = derecha!!.derecha
                izquierda = derecha!!.izquierda
            } else if (derecha == null) {
                // Nodo con un hijo izquierdo
                dato = izquierda!!.dato
                izquierda = izquierda!!.izquierda
                derecha = izquierda!!.derecha
            } else {
                // Nodo con dos hijos
                val nodoSucesor = encontrarSucesor()
                dato = nodoSucesor!!.dato
                eliminar(nodoSucesor.dato)
            }
        } else if (datoEliminar < dato) {
            if (izquierda != null) {
                izquierda!!.eliminar(datoEliminar)
            }
        } else {
            if (derecha != null) {
                derecha!!.eliminar(datoEliminar)
            }
        }
    }

    // Función para encontrar el nodo más a la izquierda del árbol
    fun encontrarMinimo(): Nodo {
        var nodoActual = this
        while (nodoActual.izquierda != null) {
            nodoActual = nodoActual.izquierda!!
        }
        return nodoActual
    }

    // Función para encontrar el nodo más a la derecha del árbol
    fun encontrarMaximo(): Nodo {
        var nodoActual = this
        while (nodoActual.derecha != null) {
            nodoActual = nodoActual.derecha!!
        }
        return nodoActual
    }

    // Función para encontrar el nodo sucesor de un nodo dado
    fun encontrarSucesor(): Nodo? {
        if (derecha != null) {
            return derecha!!.encontrarMinimo()
        } else {
            var nodoActual = this
            var nodoPadre = nodoActual.padre
            while (nodoPadre != null && nodoActual == nodoPadre.derecha) {
                nodoActual = nodoPadre
                nodoPadre = nodoPadre.padre
            }
            return nodoPadre
        }
    }

    // Función para encontrar el nodo predecesor de un nodo dado
    fun encontrarPredecesor(): Nodo? {
        if (izquierda != null) {
            return izquierda!!.encontrarMaximo()
        } else {
            var nodoActual = this
            var nodoPadre = nodoActual.padre
            while (nodoPadre != null && nodoActual == nodoPadre.izquierda) {
                nodoActual = nodoPadre
                nodoPadre = nodoPadre.padre
            }
            return nodoPadre
        }
    }

    // Función para imprimir el árbol en preorden
    fun imprimirPreorden() {
        print("$dato ")
        izquierda?.imprimirPreorden()
        derecha?.imprimirPreorden()
    }

    // Función para imprimir el árbol en inorden
    fun imprimirInorden() {
        izquierda?.imprimirInorden()
        print("$dato ")
        derecha?.imprimirInorden()
    }

    // Función para imprimir el árbol en posorden
    fun imprimirPosorden() {
        izquierda?.imprimirPosorden()
        derecha?.imprimirPosorden()
        print("$dato ")
    }
}

// Clase para representar un árbol binario
class ArbolBinario {
    var raiz: Nodo? = null

    // Función para insertar un nodo en el árbol
    fun insertar(nuevoDato: Int) {
        if (raiz == null) {
            raiz = Nodo(nuevoDato)
        } else {
            raiz!!.insertar(nuevoDato)
        }
    }

    // Función para buscar un nodo en el árbol
    fun buscar(datoBuscado: Int): Nodo? {
        return raiz?.buscar(datoBuscado)
    }

    // Función para eliminar un nodo del árbol
    fun eliminar(datoEliminar: Int) {
        raiz?.eliminar(datoEliminar)
    }

    // Función para encontrar el nodo más a la izquierda del árbol
    fun encontrarMinimo(): Nodo {
        return raiz!!.encontrarMinimo()
    }

    // Función para encontrar el nodo más a la derecha del árbol
    fun encontrarMaximo(): Nodo {
        return raiz!!.encontrarMaximo()
    }

    // Función para encontrar el nodo sucesor de un nodo dado
    fun encontrarSucesor(datoSucesor: Int): Nodo? {
        return raiz?.buscar(datoSucesor)?.encontrarSucesor()
    }

    // Función para encontrar el nodo predecesor de un nodo dado
    fun encontrarPredecesor(datoPredecesor: Int): Nodo? {
        return raiz?.buscar(datoPredecesor)?.encontrarPredecesor()
    }

    // Función para imprimir el árbol en preorden
    fun imprimirPreorden() {
        raiz?.imprimirPreorden()
        println()
    }

    // Función para imprimir el árbol en inorden
    fun imprimirInorden() {
        raiz?.imprimirInorden()
        println()
    }

    // Función para imprimir el árbol en posorden
    fun imprimirPosorden() {
        raiz?.imprimirPosorden()
        println()
    }
}

// Ejemplo de uso
val arbol = ArbolBinario()
arbol.insertar(10)
arbol.insertar(5)
arbol.insertar(15)
arbol.insertar(3)
arbol.insertar(7)
arbol.insertar(12)
arbol.insertar(20)

arbol.imprimirPreorden()  // 10 5 3 7 15 12 20
arbol.imprimirInorden()   // 3 5 7 10 12 15 20
arbol.imprimirPosorden()  // 3 7 5 12 20 15 10

val nodoBuscado = 15
val nodoEncontrado = arbol.buscar(nodoBuscado)
if (nodoEncontrado != null) {
    println("El nodo $nodoBuscado fue encontrado.")
} else {
    println("El nodo $nodoBuscado no fue encontrado.")
}

arbol.eliminar(10)
arbol.imprimirInorden()  // 3 5 7 12 15 20