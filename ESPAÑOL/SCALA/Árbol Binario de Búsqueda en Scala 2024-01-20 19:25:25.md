```scala
object CódigoComplejo {

  // Clase que representa un nodo de un árbol binario.
  class Nodo(var dato: Int, var izquierdo: Nodo, var derecho: Nodo) {
    def insertar(nuevoDato: Int): Unit = {
      if (nuevoDato < dato) {
        if (izquierdo == null) {
          izquierdo = new Nodo(nuevoDato, null, null)
        } else {
          izquierdo.insertar(nuevoDato)
        }
      } else {
        if (derecho == null) {
          derecho = new Nodo(nuevoDato, null, null)
        } else {
          derecho.insertar(nuevoDato)
        }
      }
    }

    def buscar(datoObjetivo: Int): Nodo = {
      if (dato == datoObjetivo) {
        this
      } else if (datoObjetivo < dato) {
        if (izquierdo != null) {
          izquierdo.buscar(datoObjetivo)
        } else {
          null
        }
      } else {
        if (derecho != null) {
          derecho.buscar(datoObjetivo)
        } else {
          null
        }
      }
    }

    def eliminar(datoObjetivo: Int): Unit = {
      if (datoObjetivo < dato) {
        if (izquierdo != null) {
          izquierdo.eliminar(datoObjetivo)
        }
      } else if (datoObjetivo > dato) {
        if (derecho != null) {
          derecho.eliminar(datoObjetivo)
        }
      } else {
        if (izquierdo == null && derecho == null) {
          dato = null
        } else if (izquierdo != null && derecho == null) {
          dato = izquierdo.dato
          izquierdo = izquierdo.izquierdo
          derecho = izquierdo.derecho
        } else if (izquierdo == null && derecho != null) {
          dato = derecho.dato
          izquierdo = derecho.izquierdo
          derecho = derecho.derecho
        } else {
          val nodoSucesor = buscarSucesor()
          dato = nodoSucesor.dato
          nodoSucesor.eliminar(nodoSucesor.dato)
        }
      }
    }

    def buscarSucesor(): Nodo = {
      var nodoActual = this
      var nodoSucesor = this.derecho
      while (nodoSucesor.izquierdo != null) {
        nodoActual = nodoSucesor
        nodoSucesor = nodoSucesor.izquierdo
      }
      nodoSucesor
    }

    def recorrerEnOrden(): Unit = {
      if (izquierdo != null) {
        izquierdo.recorrerEnOrden()
      }
      println(dato)
      if (derecho != null) {
        derecho.recorrerEnOrden()
      }
    }

    def recorrerPreOrden(): Unit = {
      println(dato)
      if (izquierdo != null) {
        izquierdo.recorrerPreOrden()
      }
      if (derecho != null) {
        derecho.recorrerPreOrden()
      }
    }

    def recorrerPostOrden(): Unit = {
      if (izquierdo != null) {
        izquierdo.recorrerPostOrden()
      }
      if (derecho != null) {
        derecho.recorrerPostOrden()
      }
      println(dato)
    }
  }

  // Clase que representa un árbol binario de búsqueda.
  class ÁrbolBinarioDeBúsqueda {

    var raiz: Nodo = null

    def insertar(nuevoDato: Int): Unit = {
      if (raiz == null) {
        raiz = new Nodo(nuevoDato, null, null)
      } else {
        raiz.insertar(nuevoDato)
      }
    }

    def buscar(datoObjetivo: Int): Nodo = {
      if (raiz == null) {
        null
      } else {
        raiz.buscar(datoObjetivo)
      }
    }

    def eliminar(datoObjetivo: Int): Unit = {
      if (raiz != null) {
        raiz.eliminar(datoObjetivo)
      }
    }

    def recorrerEnOrden(): Unit = {
      if (raiz != null) {
        raiz.recorrerEnOrden()
      }
    }

    def recorrerPreOrden(): Unit = {
      if (raiz != null) {
        raiz.recorrerPreOrden()
      }
    }

    def recorrerPostOrden(): Unit = {
      if (raiz != null) {
        raiz.recorrerPostOrden()
      }
    }
  }

  // Función principal.
  def main(args: Array[String]): Unit = {
    val árbol = new ÁrbolBinarioDeBúsqueda()
    árbol.insertar(10)
    árbol.insertar(5)
    árbol.insertar(15)
    árbol.insertar(2)
    árbol.insertar(7)
    árbol.insertar(12)
    árbol.insertar(20)

    println("Recorrido en orden:")
    árbol.recorrerEnOrden()

    println("Recorrido preorden:")
    árbol.recorrerPreOrden()

    println("Recorrido postorden:")
    árbol.recorrerPostOrden()

    val nodoBuscado = árbol.buscar(15)
    if (nodoBuscado != null) {
      println(s"El nodo con el dato 15 fue encontrado.")
    } else {
      println(s"El nodo con el dato 15 no fue encontrado.")
    }

    árbol.eliminar(10)
    println("Recorrido en orden después de eliminar el nodo con el dato 10:")
    árbol.recorrerEnOrden()
  }
}
```

Explicación:

El código anterior define una clase `Nodo` que representa un nodo de un árbol binario. Esta clase tiene tres propiedades: `dato`, `izquierdo` y `derecho`. La propiedad `dato` contiene el dato del nodo, la propiedad `izquierdo` contiene una referencia al nodo hijo izquierdo y la propiedad `derecho` contiene una referencia al nodo hijo derecho.

La clase `Nodo` también define varios métodos, incluyendo `insertar`, `buscar`, `eliminar`, `buscarSucesor`, `recorrerEnOrden`, `recorrerPreOrden` y `recorrerPostOrden`.

El método `insertar` inserta un nuevo dato en el árbol binario. El método `buscar` busca un dato en el árbol binario. El método `eliminar` elimina un dato del árbol binario. El método `buscarSucesor` busca el nodo sucesor de un nodo dado.

Los métodos `recorrerEnOrden`, `recorrerPreOrden` y `recorrerPostOrden` recorren el árbol binario en orden, preorden y postorden respectivamente.

El código también define una clase `ÁrbolBinarioDeBúsqueda` que representa un árbol binario de búsqueda. Esta clase tiene una propiedad `raiz` que contiene una referencia al nodo raíz del árbol binario de búsqueda.

La clase `ÁrbolBinarioDeBúsqueda` también define varios métodos, incluyendo `insertar`, `buscar`, `eliminar`, `recorrerEnOrden`, `recorrerPreOrden` y `recorrerPostOrden`. Estos métodos son similares a los métodos definidos en la clase `Nodo`.

La función `main` es el punto de entrada al programa. Esta función crea un objeto de la clase `ÁrbolBinarioDeBúsqueda` y luego llama a los métodos `insertar`, `buscar`, `eliminar` y `recorrer` para demostrar cómo usar el árbol binario de búsqueda.