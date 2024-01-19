```cool
clase Lista {
  atributos:
    elementos: Array[Object]
  métodos:
    agregar(elemento: Object): Unit
      elementos.agregar(elemento)
    eliminar(elemento: Object): Unit
      elementos.eliminar(elemento)
    obtener(índice: Integer): Object
      return elementos[índice]
    tamaño(): Integer
      return elementos.tamaño()
    estáVacía(): Boolean
      return elementos.tamaño() == 0
}

clase Pila {
  atributos:
    elementos: Array[Object]
  métodos:
    apilar(elemento: Object): Unit
      elementos.agregar(elemento)
    desapilar(): Object
      return elementos.eliminar(elementos.tamaño() - 1)
    cima(): Object
      return elementos[elementos.tamaño() - 1]
    tamaño(): Integer
      return elementos.tamaño()
    estáVacía(): Boolean
      return elementos.tamaño() == 0
}

clase Cola {
  atributos:
    elementos: Array[Object]
  métodos:
    encolar(elemento: Object): Unit
      elementos.agregar(elemento)
    desencolar(): Object
      return elementos.eliminar(0)
    frente(): Object
      return elementos[0]
    tamaño(): Integer
      return elementos.tamaño()
    estáVacía(): Boolean
      return elementos.tamaño() == 0
}

clase ÁrbolBinario {
  atributos:
    valor: Object
    izquierda: ÁrbolBinario
    derecha: ÁrbolBinario
  métodos:
    insertar(elemento: Object): Unit
      if elemento < valor then
        if izquierda == null then
          izquierda = new ÁrbolBinario(elemento)
        else
          izquierda.insertar(elemento)
      else
        if derecha == null then
          derecha = new ÁrbolBinario(elemento)
        else
          derecha.insertar(elemento)
    buscar(elemento: Object): Boolean
      if elemento == valor then
        return true
      else if elemento < valor and izquierda != null then
        return izquierda.buscar(elemento)
      else if elemento > valor and derecha != null then
        return derecha.buscar(elemento)
      else
        return false
    recorrerEnOrden(visitante: Visitor): Unit
      if izquierda != null then
        izquierda.recorrerEnOrden(visitante)
      visitante.visitar(valor)
      if derecha != null then
        derecha.recorrerEnOrden(visitante)
    recorrerPreOrden(visitante: Visitor): Unit
      visitante.visitar(valor)
      if izquierda != null then
        izquierda.recorrerPreOrden(visitante)
      if derecha != null then
        derecha.recorrerPreOrden(visitante)
    recorrerPostOrden(visitante: Visitor): Unit
      if izquierda != null then
        izquierda.recorrerPostOrden(visitante)
      if derecha != null then
        derecha.recorrerPostOrden(visitante)
      visitante.visitar(valor)
}

clase Grafo {
  atributos:
    vertices: Array[Vertex]
    aristas: Array[Edge]
  métodos:
    agregarVértice(vertex: Vertex): Unit
      vertices.agregar(vertex)
    agregarArista(edge: Edge): Unit
      aristas.agregar(edge)
    obtenerVértice(nombre: String): Vertex
      for vertex in vertices do
        if vertex.nombre == nombre then
          return vertex
      return null
    obtenerArista(u: Vertex, v: Vertex): Edge
      for edge in aristas do
        if edge.u == u and edge.v == v then
          return edge
      return null
    recorrerAnchuraPrimero(vérticeInicial: Vertex, visitante: Visitor): Unit
      cola = new Cola()
      cola.encolar(vérticeInicial)
      while not cola.estáVacía() do
        vérticeActual = cola.desencolar()
        visitante.visitar(vérticeActual)
        for vecino in vérticeActual.vecinos do
          cola.encolar(vecino)
    recorrerProfundidadPrimero(vérticeInicial: Vertex, visitante: Visitor): Unit
      pila = new Pila()
      pila.apilar(vérticeInicial)
      while not pila.estáVacía() do
        vérticeActual = pila.desapilar()
        visitante.visitar(vérticeActual)
        for vecino in vérticeActual.vecinos do
          pila.apilar(vecino)
}

clase Vértice {
  atributos:
    nombre: String
    vecinos: Array[Vertex]
  métodos:
    agregarVecino(vecino: Vertex): Unit
      vecinos.agregar(vecino)
}

clase Arista {
  atributos:
    u: Vertex
    v: Vertex
    peso: Integer
  métodos:
    obtenerPeso(): Integer
      return peso
}

clase Visitante {
  métodos:
    visitar(elemento: Object): Unit
      // Método abstracto que será implementado por las clases concretas
}

// Ejemplo de uso de las clases definidas

lista = new Lista()
lista.agregar(1)
lista.agregar(2)
lista.agregar(3)

for i in 0 to lista.tamaño() - 1 do
  println(lista.obtener(i))

cola = new Cola()
cola.encolar(1)
cola.encolar(2)
cola.encolar(3)

while not cola.estáVacía() do
  println(cola.desencolar())

pila = new Pila()
pila.apilar(1)
pila.apilar(2)
pila.apilar(3)

while not pila.estáVacía() do
  println(pila.desapilar())

árbol = new ÁrbolBinario(10)
árbol.insertar(5)
árbol.insertar(15)
árbol.insertar(2)
árbol.insertar(7)
árbol.insertar(12)
árbol.insertar(20)

árbol.recorrerEnOrden({ |valor| println(valor) })
árbol.recorrerPreOrden({ |valor| println(valor) })
árbol.recorrerPostOrden({ |valor| println(valor) })

grafo = new Grafo()
vérticeA = new Vértice("A")
vérticeB = new Vértice("B")
vérticeC = new Vértice("C")
vérticeD = new Vértice("D")
vérticeE = new Vértice("E")
aristaAB = new Arista(vérticeA, vérticeB, 1)
aristaAC = new Arista(vérticeA, vérticeC, 2)
aristaBC = new Arista(vérticeB, vérticeC, 3)
aristaCD = new Arista(vérticeC, vérticeD, 4)
aristaDE = new Arista(vérticeD, vérticeE, 5)
grafo.agregarVértice(vérticeA)
grafo.agregarVértice(vérticeB)
grafo.agregarVértice(vérticeC)
grafo.agregarVértice(vérticeD)
grafo.agregarVértice(vérticeE)
grafo.agregarArista(aristaAB)
grafo.agregarArista(aristaAC)
grafo.agregarArista(aristaBC)
grafo.agregarArista(aristaCD)
grafo.agregarArista(aristaDE)

grafo.recorrerAnchuraPrimero(vérticeA, { |vértice| println(vértice.nombre) })
grafo.recorrerProf