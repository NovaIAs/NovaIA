```swift
// Implementación de un árbol binario de búsqueda en Swift

public class Nodo<T: Comparable> {
  var valor: T
  var izquierdo: Nodo<T>?
  var derecho: Nodo<T>?

  public init(valor: T) {
    self.valor = valor
  }
}

public class ArbolBinarioDeBusqueda<T: Comparable> {
  var raiz: Nodo<T>?

  public func insertar(valor: T) {
    if raiz == nil {
      raiz = Nodo<T>(valor: valor)
      return
    }

    var actual = raiz!
    while true {
      if valor < actual.valor {
        if actual.izquierdo == nil {
          actual.izquierdo = Nodo<T>(valor: valor)
          return
        } else {
          actual = actual.izquierdo!
        }
      } else {
        if actual.derecho == nil {
          actual.derecho = Nodo<T>(valor: valor)
          return
        } else {
          actual = actual.derecho!
        }
      }
    }
  }

  public func buscar(valor: T) -> Nodo<T>? {
    var actual = raiz
    while actual != nil {
      if valor == actual!.valor {
        return actual
      } else if valor < actual!.valor {
        actual = actual!.izquierdo
      } else {
        actual = actual!.derecho
      }
    }
    return nil
  }

  public func eliminar(valor: T) {
    var padre: Nodo<T>?
    var actual = raiz
    while actual != nil {
      if valor == actual!.valor {
        break
      } else if valor < actual!.valor {
        padre = actual
        actual = actual!.izquierdo
      } else {
        padre = actual
        actual = actual!.derecho
      }
    }

    if actual == nil {
      return
    }

    if actual!.izquierdo == nil {
      if actual == raiz {
        raiz = actual!.derecho
      } else if padre!.izquierdo == actual {
        padre!.izquierdo = actual!.derecho
      } else {
        padre!.derecho = actual!.derecho
      }
    } else if actual!.derecho == nil {
      if actual == raiz {
        raiz = actual!.izquierdo
      } else if padre!.izquierdo == actual {
        padre!.izquierdo = actual!.izquierdo
      } else {
        padre!.derecho = actual!.izquierdo
      }
    } else {
      var sucesor = actual!.derecho
      var padreSucesor: Nodo<T>?
      while sucesor!.izquierdo != nil {
        padreSucesor = sucesor
        sucesor = sucesor!.izquierdo
      }

      if padreSucesor != nil {
        padreSucesor!.izquierdo = sucesor!.derecho
      } else {
        actual!.derecho = sucesor!.derecho
      }

      actual!.valor = sucesor!.valor
    }
  }

  public func recorrerEnPreOrden(nodo: Nodo<T>? = nil, visita: (T) -> ()) {
    if nodo == nil {
      nodo = raiz
    }

    if nodo != nil {
      visita(nodo!.valor)
      recorrerEnPreOrden(nodo: nodo!.izquierdo, visita: visita)
      recorrerEnPreOrden(nodo: nodo!.derecho, visita: visita)
    }
  }

  public func recorrerEnInOrden(nodo: Nodo<T>? = nil, visita: (T) -> ()) {
    if nodo == nil {
      nodo = raiz
    }

    if nodo != nil {
      recorrerEnInOrden(nodo: nodo!.izquierdo, visita: visita)
      visita(nodo!.valor)
      recorrerEnInOrden(nodo: nodo!.derecho, visita: visita)
    }
  }

  public func recorrerEnPostOrden(nodo: Nodo<T>? = nil, visita: (T) -> ()) {
    if nodo == nil {
      nodo = raiz
    }

    if nodo != nil {
      recorrerEnPostOrden(nodo: nodo!.izquierdo, visita: visita)
      recorrerEnPostOrden(nodo: nodo!.derecho, visita: visita)
      visita(nodo!.valor)
    }
  }
}

// Ejemplo de uso

let arbol = ArbolBinarioDeBusqueda<Int>()
arbol.insertar(valor: 10)
arbol.insertar(valor: 5)
arbol.insertar(valor: 15)
arbol.insertar(valor: 2)
arbol.insertar(valor: 7)
arbol.insertar(valor: 12)
arbol.insertar(valor: 20)

print("Recorrido en pre-orden:")
arbol.recorrerEnPreOrden { valor in
  print(valor)
}

print("Recorrido en in-orden:")
arbol.recorrerEnInOrden { valor in
  print(valor)
}

print("Recorrido en post-orden:")
arbol.recorrerEnPostOrden { valor in
  print(valor)
}

if let nodoBuscado = arbol.buscar(valor: 12) {
  print("El nodo con valor 12 se encontró:")
  print("Valor:", nodoBuscado.valor)
} else {
  print("El nodo con valor 12 no se encontró.")
}

arbol.eliminar(valor: 10)

print("El árbol después de eliminar el nodo con valor 10:")
arbol.recorrerEnPreOrden { valor in
  print(valor)
}
```

Explicación del código:

* La clase `Nodo` representa un nodo del árbol binario de búsqueda. Tiene las siguientes propiedades:
    * `valor`: El valor del nodo.
    * `izquierdo`: El nodo hijo izquierdo del nodo actual.
    * `derecho`: El nodo hijo derecho del nodo actual.
* La clase `ArbolBinarioDeBusqueda` representa un árbol binario de búsqueda. Tiene las siguientes propiedades:
    * `raiz`: El nodo raíz del árbol.
* El método `insertar` inserta un nuevo nodo en el árbol. Si el árbol está vacío, el nuevo nodo se convierte en la raíz. De lo contrario, el nodo se inserta en la rama izquierda o derecha del nodo padre adecuado, dependiendo del valor del nuevo nodo.
* El método `buscar` busca un nodo en el árbol con el valor especificado. Si el nodo se encuentra, se devuelve. De lo contrario, se devuelve `nil`.
* El método `eliminar` elimina un nodo del árbol con el valor especificado. Si el nodo no se encuentra, no se hace nada. De lo contrario, el nodo se elimina del árbol y sus hijos se reordenan para mantener la propiedad de árbol binario de búsqueda.
* Los métodos `recorrerEnPreOrden`, `recorrerEnInOrden` y `recorrerEnPostOrden` recorren el árbol en pre-orden, in-orden y post-orden respectivamente. El parámetro `visita` es una función que se llama para cada nodo visitado.
* El ejemplo de uso muestra cómo crear un árbol binario de búsqueda, insertar y eliminar nodos, buscar un nodo y recorrer el árbol en diferentes órdenes.