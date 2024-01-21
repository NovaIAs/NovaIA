```scala
import scala.collection.mutable.ListBuffer

object CodigoComplejo {

  def main(args: Array[String]): Unit = {
    // Creamos una clase para representar un nodo de un árbol binario.
    class NodoBinario(var valor: Int, var izquierda: NodoBinario, var derecha: NodoBinario) {
      // Métodos getters y setters para los atributos valor, izquierda y derecha.
      def getValor(): Int = valor
      def setValor(valor: Int): Unit = this.valor = valor
      def getIzquierda(): NodoBinario = izquierda
      def setIzquierda(izquierda: NodoBinario): Unit = this.izquierda = izquierda
      def getDerecha(): NodoBinario = derecha
      def setDerecha(derecha: NodoBinario): Unit = this.derecha = derecha
    }

    // Creamos una clase para representar un árbol binario.
    class ArbolBinario {
      // Atributo raíz que apunta al nodo raíz del árbol.
      var raiz: NodoBinario = _

      // Método para insertar un nuevo nodo en el árbol.
      def insertar(valor: Int): Unit = {
        // Creamos un nuevo nodo con el valor dado.
        val nuevoNodo = new NodoBinario(valor, null, null)

        // Si el árbol está vacío, el nuevo nodo se convierte en la raíz.
        if (raiz == null) {
          raiz = nuevoNodo
        } else {
          // Si el árbol no está vacío, buscamos la posición adecuada para insertar el nuevo nodo.
          var nodoActual: NodoBinario = raiz
          var padre: NodoBinario = null

          while (nodoActual != null) {
            padre = nodoActual
            if (valor < nodoActual.getValor()) {
              nodoActual = nodoActual.getIzquierda()
            } else {
              nodoActual = nodoActual.getDerecha()
            }
          }

          // Insertamos el nuevo nodo como hijo izquierdo o derecho del padre.
          if (valor < padre.getValor()) {
            padre.setIzquierda(nuevoNodo)
          } else {
            padre.setDerecha(nuevoNodo)
          }
        }
      }

      // Método para eliminar un nodo del árbol.
      def eliminar(valor: Int): Unit = {
        // Buscamos el nodo a eliminar y su padre.
        var nodoActual: NodoBinario = raiz
        var padre: NodoBinario = null

        while (nodoActual != null && nodoActual.getValor() != valor) {
          padre = nodoActual
          if (valor < nodoActual.getValor()) {
            nodoActual = nodoActual.getIzquierda()
          } else {
            nodoActual = nodoActual.getDerecha()
          }
        }

        // Si el nodo a eliminar no se encuentra, salimos del método.
        if (nodoActual == null) {
          return
        }

        // Si el nodo a eliminar es una hoja, simplemente lo eliminamos.
        if (nodoActual.getIzquierda() == null && nodoActual.getDerecha() == null) {
          if (padre == null) {
            raiz = null
          } else if (nodoActual == padre.getIzquierda()) {
            padre.setIzquierda(null)
          } else {
            padre.setDerecha(null)
          }
        } else if (nodoActual.getDerecha() == null) {
          // Si el nodo a eliminar tiene un solo hijo izquierdo, lo hacemos hijo del padre del nodo a eliminar.
          if (padre == null) {
            raiz = nodoActual.getIzquierda()
          } else if (nodoActual == padre.getIzquierda()) {
            padre.setIzquierda(nodoActual.getIzquierda())
          } else {
            padre.setDerecha(nodoActual.getIzquierda())
          }
        } else if (nodoActual.getIzquierda() == null) {
          // Si el nodo a eliminar tiene un solo hijo derecho, lo hacemos hijo del padre del nodo a eliminar.
          if (padre == null) {
            raiz = nodoActual.getDerecha()
          } else if (nodoActual == padre.getIzquierda()) {
            padre.setIzquierda(nodoActual.getDerecha())
          } else {
            padre.setDerecha(nodoActual.getDerecha())
          }
        } else {
          // Si el nodo a eliminar tiene dos hijos, buscamos el nodo más pequeño en el subárbol derecho del nodo a eliminar y lo hacemos hijo del padre del nodo a eliminar.
          var nodoMasPequeno = nodoActual.getDerecha()
          var padreNodoMasPequeno: NodoBinario = nodoActual

          while (nodoMasPequeno.getIzquierda() != null) {
            padreNodoMasPequeno = nodoMasPequeno
            nodoMasPequeno = nodoMasPequeno.getIzquierda()
          }

          nodoActual.setValor(nodoMasPequeno.getValor())

          if (padreNodoMasPequeno == nodoActual) {
            nodoActual.setDerecha(nodoMasPequeno.getDerecha())
          } else {
            padreNodoMasPequeno.setIzquierda(nodoMasPequeno.getDerecha())
          }
        }
      }

      // Método para buscar un nodo en el árbol.
      def buscar(valor: Int): NodoBinario = {
        var nodoActual: NodoBinario = raiz

        while (nodoActual != null && nodoActual.getValor() != valor) {
          if (valor < nodoActual.getValor()) {
            nodoActual = nodoActual.getIzquierda()
          } else {
            nodoActual = nodoActual.getDerecha()