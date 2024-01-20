```scala
// Importar las bibliotecas necesarias
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

// Definir una clase para representar un nodo de un árbol
class Nodo(var valor: Int, var hijos: ArrayBuffer[Nodo])

// Definir una clase para representar un árbol binario
class ArbolBinario {

  // El nodo raíz del árbol
  var raiz: Nodo = _

  // Insertar un nuevo nodo en el árbol
  def insertar(valor: Int): Unit = {

    // Si el árbol está vacío, crear el nodo raíz
    if (raiz == null) {
      raiz = new Nodo(valor, ArrayBuffer())
      return
    }

    // Buscar el nodo padre del nuevo nodo
    var nodoPadre: Nodo = null
    breakable {
      var nodoActual = raiz
      while (nodoActual != null) {
        if (valor <= nodoActual.valor) {
          nodoPadre = nodoActual
          nodoActual = nodoActual.hijos(0)
        } else {
          nodoPadre = nodoActual
          nodoActual = nodoActual.hijos(1)
        }
      }
    }

    // Crear el nuevo nodo y añadirlo al árbol
    val nuevoNodo = new Nodo(valor, ArrayBuffer())
    if (valor <= nodoPadre.valor) {
      nodoPadre.hijos(0) = nuevoNodo
    } else {
      nodoPadre.hijos(1) = nuevoNodo
    }
  }

  // Buscar un nodo en el árbol
  def buscar(valor: Int): Nodo = {

    // Si el árbol está vacío, devolver null
    if (raiz == null) {
      return null
    }

    // Buscar el nodo en el árbol
    var nodoActual = raiz
    breakable {
      while (nodoActual != null) {
        if (valor == nodoActual.valor) {
          break
        } else if (valor < nodoActual.valor) {
          nodoActual = nodoActual.hijos(0)
        } else {
          nodoActual = nodoActual.hijos(1)
        }
      }
    }

    // Devolver el nodo encontrado o null si no se encontró
    nodoActual
  }

  // Eliminar un nodo del árbol
  def eliminar(valor: Int): Unit = {

    // Si el árbol está vacío, no hacer nada
    if (raiz == null) {
      return
    }

    // Buscar el nodo que se va a eliminar y su nodo padre
    var nodoEliminar: Nodo = null
    var nodoPadre: Nodo = null
    breakable {
      var nodoActual = raiz
      while (nodoActual != null) {
        if (valor == nodoActual.valor) {
          nodoEliminar = nodoActual
          break
        } else if (valor < nodoActual.valor) {
          nodoPadre = nodoActual
          nodoActual = nodoActual.hijos(0)
        } else {
          nodoPadre = nodoActual
          nodoActual = nodoActual.hijos(1)
        }
      }
    }

    // Si el nodo a eliminar no se encontró, no hacer nada
    if (nodoEliminar == null) {
      return
    }

    // Si el nodo a eliminar es el nodo raíz, hacer que el árbol esté vacío
    if (nodoPadre == null) {
      raiz = null
    } else {

      // Eliminar el nodo a eliminar del árbol
      if (nodoEliminar == nodoPadre.hijos(0)) {
        nodoPadre.hijos(0) = null
      } else {
        nodoPadre.hijos(1) = null
      }

      // Si el nodo a eliminar tiene dos hijos, encontrar el sucesor del nodo a eliminar y reemplazarlo
      if (nodoEliminar.hijos.length == 2) {

        var sucesor: Nodo = nodoEliminar.hijos(1)
        breakable {
          while (sucesor.hijos(0) != null) {
            sucesor = sucesor.hijos(0)
          }
        }

        nodoEliminar.valor = sucesor.valor
        sucesor.valor = valor

        nodoPadre = sucesor
        breakable {
          var nodoActual = raiz
          while (nodoActual != null) {
            if (sucesor == nodoActual.hijos(1)) {
              nodoActual.hijos(1) = null
              break
            } else {
              nodoActual = nodoActual.hijos(0)
            }
          }
        }
      }
    }
  }

  // Recorrer el árbol en orden
  def recorrerEnOrden(nodo: Nodo): Unit = {

    // Si el nodo es nulo, no hacer nada
    if (nodo == null) {
      return
    }

    // Recorrer el subárbol izquierdo
    recorrerEnOrden(nodo.hijos(0))

    // Imprimir el valor del nodo actual
    println(nodo.valor)

    // Recorrer el subárbol derecho
    recorrerEnOrden(nodo.hijos(1))
  }

  // Recorrer el árbol en preorden
  def recorrerEnPreorden(nodo: Nodo): Unit = {

    // Si el nodo es nulo, no hacer nada
    if (nodo == null) {
      return
    }

    // Imprimir el valor del nodo actual
    println(nodo.valor)

    // Recorrer el subárbol izquierdo
    recorrerEnPreorden(nodo.hijos(0))

    // Recorrer el subárbol derecho
    recorrerEnPreorden(nodo.hijos(1))
  }

  // Recorrer el árbol en postorden
  def recorrerEnPostorden(nodo: Nodo): Unit = {

    // Si el nodo es nulo, no hacer nada
    if (nodo == null) {
      return
    }

    // Recorrer el subárbol izquierdo
    recorrerEnPostorden(nodo.hijos(0))

    // Recorrer el subárbol derecho
    recorrerEnPostorden(nodo.hijos(1))

    // Imprimir el valor del nodo actual
    println(nodo.valor)
  }
}

// Crear un árbol binario de ejemplo
val arbol = new ArbolBinario()
arbol.insertar(50)
arbol.insertar(30)
arbol.insertar(70)
arbol.insertar(20)
arbol.insertar(40)
arbol.insertar(60)
arbol.insertar(80)

// Buscar un nodo en el árbol
val nodoBuscado = arbol.buscar(40)
println(s"Nodo encontrado: ${nodoBuscado.valor}")

// Eliminar un nodo del árbol
arbol.eliminar(30)

// Recorrer el árbol en orden
println("Recorrido en orden:")
arbol.recorrerEnOrden(arbol.raiz)

// Recorrer el árbol en preorden
println("Recorrido en preorden:")
arbol.recorrerEnPreorden(arbol.raiz)

// Recorrer el árbol en postorden
println("Recorrido en postorden:")
arbol.recorrerEnPostorden(arbol.raiz)
```

Explicación del código:

* Se define una clase `Nodo` para representar un nodo de un árbol, con los atributos `valor` e `hijos`.
* Se define una clase `ArbolBinario` para representar un árbol binario, con el atributo `raiz`.
* Se implementan los métodos `insertar`, `buscar`, `eliminar`, `recorrerEnOrden`, `recorrerEnPreorden` y `recorrerEnPostorden` para manipular y recorrer el árbol binario.
* Se crea un árbol binario de ejemplo y se insertan algunos valores en él.
* Se busca un nodo en el árbol y se imprime su valor.
* Se elimina un nodo del árbol.
* Se recorre el árbol en orden, en preorden y en postorden, e imprime el valor de cada nodo.