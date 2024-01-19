```scala
// Clase para representar un árbol binario de búsqueda
class ÁrbolBinario[T <: Comparable[T]] {
  var valor: T
  var izquierdo: ÁrbolBinario[T]
  var derecho: ÁrbolBinario[T]

  def this() {
    this(null, null, null)
  }

  def this(valor: T) {
    this(valor, null, null)
  }

  def this(valor: T, izquierdo: ÁrbolBinario[T], derecho: ÁrbolBinario[T]) {
    this.valor = valor
    this.izquierdo = izquierdo
    this.derecho = derecho
  }

  // Método para insertar un valor en el árbol
  def insertar(valor: T) {
    if (valor == null) {
      throw new IllegalArgumentException("El valor a insertar no puede ser nulo")
    }

    if (valor.compareTo(this.valor) < 0) {
      if (this.izquierdo == null) {
        this.izquierdo = new ÁrbolBinario[T](valor)
      } else {
        this.izquierdo.insertar(valor)
      }
    } else if (valor.compareTo(this.valor) > 0) {
      if (this.derecho == null) {
        this.derecho = new ÁrbolBinario[T](valor)
      } else {
        this.derecho.insertar(valor)
      }
    }
  }

  // Método para buscar un valor en el árbol
  def buscar(valor: T): ÁrbolBinario[T] = {
    if (valor == null) {
      throw new IllegalArgumentException("El valor a buscar no puede ser nulo")
    }

    if (valor.compareTo(this.valor) == 0) {
      return this
    } else if (valor.compareTo(this.valor) < 0) {
      if (this.izquierdo == null) {
        return null
      } else {
        return this.izquierdo.buscar(valor)
      }
    } else {
      if (this.derecho == null) {
        return null
      } else {
        return this.derecho.buscar(valor)
      }
    }
  }

  // Método para eliminar un valor del árbol
  def eliminar(valor: T) {
    if (valor == null) {
      throw new IllegalArgumentException("El valor a eliminar no puede ser nulo")
    }

    if (valor.compareTo(this.valor) == 0) {
      if (this.izquierdo == null && this.derecho == null) {
        this.valor = null
      } else if (this.izquierdo != null && this.derecho == null) {
        this.valor = this.izquierdo.valor
        this.izquierdo = this.izquierdo.izquierdo
        this.derecho = this.izquierdo.derecho
      } else if (this.izquierdo == null && this.derecho != null) {
        this.valor = this.derecho.valor
        this.izquierdo = this.derecho.izquierdo
        this.derecho = this.derecho.derecho
      } else {
        var nodoPredecesor = this.predecesor()
        this.valor = nodoPredecesor.valor
        nodoPredecesor.eliminar(nodoPredecesor.valor)
      }
    } else if (valor.compareTo(this.valor) < 0) {
      if (this.izquierdo != null) {
        this.izquierdo.eliminar(valor)
      }
    } else {
      if (this.derecho != null) {
        this.derecho.eliminar(valor)
      }
    }
  }

  // Método para obtener el nodo predecesor del nodo actual
  def predecesor(): ÁrbolBinario[T] = {
    var nodoPredecesor = this.izquierdo

    while (nodoPredecesor.derecho != null) {
      nodoPredecesor = nodoPredecesor.derecho
    }

    return nodoPredecesor
  }

  // Método para obtener el nodo sucesor del nodo actual
  def sucesor(): ÁrbolBinario[T] = {
    var nodoSucesor = this.derecho

    while (nodoSucesor.izquierdo != null) {
      nodoSucesor = nodoSucesor.izquierdo
    }

    return nodoSucesor
  }

  // Método para imprimir el árbol en orden
  def imprimirEnOrden() {
    if (this.izquierdo != null) {
      this.izquierdo.imprimirEnOrden()
    }

    print(this.valor + " ")

    if (this.derecho != null) {
      this.derecho.imprimirEnOrden()
    }
  }

  // Método para imprimir el árbol en preorden
  def imprimirEnPreorden() {
    print(this.valor + " ")

    if (this.izquierdo != null) {
      this.izquierdo.imprimirEnPreorden()
    }

    if (this.derecho != null) {
      this.derecho.imprimirEnPreorden()
    }
  }

  // Método para imprimir el árbol en postorden
  def imprimirEnPostorden() {
    if (this.izquierdo != null) {
      this.izquierdo.imprimirEnPostorden()
    }

    if (this.derecho != null) {
      this.derecho.imprimirEnPostorden()
    }

    print(this.valor + " ")
  }
}

// Clase principal para probar el árbol binario de búsqueda
object ÁrbolBinarioMain {
  def main(args: Array[String]) {
    // Crear un árbol binario de búsqueda
    var árbol = new ÁrbolBinario[Integer]()

    // Insertar valores en el árbol
    árbol.insertar(10)
    árbol.insertar(5)
    árbol.insertar(15)
    árbol.insertar(2)
    árbol.insertar(7)
    árbol.insertar(12)
    árbol.insertar(20)

    // Imprimir el árbol en orden
    println("Imprimiendo el árbol en orden:")
    árbol.imprimirEnOrden()
    println()

    // Imprimir el árbol en preorden
    println("Imprimiendo el árbol en preorden:")
    árbol.imprimirEnPreorden()
    println()

    // Imprimir el árbol en postorden
    println("Imprimiendo el árbol en postorden:")
    árbol.imprimirEnPostorden()
    println()

    // Buscar un valor en el árbol
    var valorABuscar = 12
    var nodoBuscado = árbol.buscar(valorABuscar)

    if (nodoBuscado != null) {
      println("El valor " + valorABuscar + " se encuentra en el árbol")
    } else {
      println("El valor " + valorABuscar + " no se encuentra en el árbol")
    }

    // Eliminar un valor del árbol
    var valorAEliminar = 7
    árbol.eliminar(valorAEliminar)

    // Imprimir el árbol en orden después de eliminar el valor
    println("Imprimiendo el árbol en orden después de eliminar el valor " + valorAEliminar + ":")
    árbol.imprimirEnOrden()
    println()

    // Obtener el nodo predecesor del nodo actual
    var nodoPredecesor = árbol.predecesor()
    println("El nodo predecesor del nodo actual es: " + nodoPredecesor.valor)

    // Obtener el nodo sucesor del nodo actual
    var nodoSucesor = árbol.sucesor()
    println("El nodo sucesor del nodo actual es: " + nodoSucesor.valor)
  }
}
```

Explicación del código:

* La clase `ÁrbolBinario` representa un árbol binario de búsqueda.
* La clase tiene tres propiedades: `valor`, `izquierdo` y `derecho`.
* La propiedad `valor` almacena el valor del nodo actual.
* La propiedad `izquierdo` almacena el nodo hijo izquierdo del nodo actual.
* La propiedad `derecho` almacena el nodo hijo derecho del nodo actual.
* El constructor de la clase `ÁrbolBinario` toma tres parámetros: `valor`, `izquierdo` y `derecho`.
* El método `insertar` inserta un valor en el árbol.
* El método `buscar` busca un valor en el árbol.
* El método `eliminar` elimina un valor del árbol.
* El método `predecesor` obtiene el nodo predecesor del nodo actual.
* El método `sucesor` obtiene el nodo sucesor del nodo actual.
* El método `imprimirEnOrden` imprime el árbol en orden.
* El método `imprimirEnPreorden` imprime el árbol en preorden.
* El método `imprimirEnPostorden` imprime el árbol en postorden.
* La clase `ÁrbolBinarioMain` es la clase principal para probar el árbol binario de búsqueda.
* El método `main` de la clase `ÁrbolBinarioMain` crea un árbol binario de búsqueda, inserta valores en el árbol, imprime el árbol en orden, preorden y postorden, busca un valor en el árbol, elimina un valor del árbol, obtiene el nodo predecesor y sucesor del nodo actual, e imprime el árbol en orden después de eliminar el valor.