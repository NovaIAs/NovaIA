```cool

-- Definición de la clase `Lista`.
class Lista {
  -- Atributos privados de la clase.
  private var nodo_cabeza: Nodo o nada;
  private var nodo_cola: Nodo o nada;
  private var tamaño: entero;

  -- Constructor de la clase.
  constructor crea_nueva_lista() {
    nodo_cabeza := nada;
    nodo_cola := nada;
    tamaño := 0;
  }

  -- Método para agregar un nodo al final de la lista.
  procedure agregar_nodo_al_final(elemento: entero) {
    var nuevo_nodo: Nodo := crea_nuevo_nodo(elemento);

    -- Si la lista está vacía, el nuevo nodo se convierte en la cabeza y la cola.
    si (nodo_cabeza es nada) entonces {
      nodo_cabeza := nuevo_nodo;
      nodo_cola := nuevo_nodo;
    }

    -- De lo contrario, agregamos el nuevo nodo al final de la lista.
    sino {
      nodo_cola.siguiente := nuevo_nodo;
      nodo_cola := nuevo_nodo;
    }

    -- Incrementamos el tamaño de la lista.
    tamaño := tamaño + 1;
  }

  -- Método para eliminar el nodo al final de la lista.
  procedure eliminar_nodo_del_final() {
    -- Si la lista está vacía, no hacemos nada.
    si (nodo_cabeza es nada) entonces {
      return;
    }

    -- Si la lista tiene un solo nodo, lo eliminamos y ponemos la cabeza y la cola en nada.
    si (tamaño es 1) entonces {
      nodo_cabeza := nada;
      nodo_cola := nada;
      tamaño := 0;

      return;
    }

    -- De lo contrario, recorremos la lista hasta llegar al nodo anterior al último.
    var nodo_anterior: Nodo := nodo_cabeza;
    mientras (nodo_anterior.siguiente no es nodo_cola) {
      nodo_anterior := nodo_anterior.siguiente;
    }

    -- Eliminamos el último nodo de la lista.
    nodo_anterior.siguiente := nada;
    nodo_cola := nodo_anterior;

    -- Decrementamos el tamaño de la lista.
    tamaño := tamaño - 1;
  }

  -- Método para obtener el tamaño de la lista.
  procedure obtener_tamaño(): entero {
    devuelve tamaño;
  }

  -- Método para obtener el elemento en un índice específico de la lista.
  procedure obtener_elemento_en_índice(índice: entero): entero {
    -- Verificamos si el índice está dentro de los límites de la lista.
    si (índice < 0 o índice >= tamaño) entonces {
      error("Índice fuera de los límites de la lista.");
    }

    -- Recorremos la lista hasta llegar al nodo correspondiente al índice.
    var nodo_actual: Nodo := nodo_cabeza;
    para índice_actual de 0 hasta índice hacer {
      nodo_actual := nodo_actual.siguiente;
    }

    -- Devolvemos el elemento en el nodo correspondiente al índice.
    devuelve nodo_actual.elemento;
  }

  -- Método para buscar un elemento en la lista.
  procedure buscar_elemento(elemento: entero): booleano {
    var nodo_actual: Nodo := nodo_cabeza;

    -- Recorremos la lista hasta encontrar el elemento o llegar al final de la lista.
    mientras (nodo_actual no es nada) {
      si (nodo_actual.elemento es elemento) entonces {
        devuelve verdadero;
      }

      nodo_actual := nodo_actual.siguiente;
    }

    -- Si no encontramos el elemento, devolvemos falso.
    devuelve falso;
  }

  -- Método para ordenar la lista en orden ascendente.
  procedure ordenar() {
    -- Si la lista está vacía o tiene un solo nodo, no hacemos nada.
    si (tamaño <= 1) entonces {
      return;
    }

    -- Creamos una lista auxiliar para almacenar los elementos ordenados.
    var lista_ordenada: Lista := crea_nueva_lista();

    -- Recorremos la lista original y añadimos cada elemento a la lista ordenada en su posición correspondiente.
    mientras (nodo_cabeza no es nada) {
      var elemento_actual: entero := nodo_cabeza.elemento;
      var nodo_anterior: Nodo o nada := nada;
      var nodo_actual: Nodo := lista_ordenada.nodo_cabeza;

      -- Recorremos la lista ordenada hasta encontrar el nodo anterior al que corresponde el elemento actual.
      mientras (nodo_actual no es nada y elemento_actual > nodo_actual.elemento) {
        nodo_anterior := nodo_actual;
        nodo_actual := nodo_actual.siguiente;
      }

      -- Si el nodo anterior es nada, significa que el elemento actual es el menor de la lista ordenada.
      si (nodo_anterior es nada) entonces {
        lista_ordenada.agregar_nodo_al_final(elemento_actual);
      }

      -- De lo contrario, insertamos el elemento actual entre el nodo anterior y el nodo actual.
      si no {
        var nuevo_nodo: Nodo := crea_nuevo_nodo(elemento_actual);

        nodo_anterior.siguiente := nuevo_nodo;
        nuevo_nodo.siguiente := nodo_actual;

        -- Si el nodo actual es la cabeza de la lista ordenada, actualizamos la cabeza.
        si (nodo_actual es lista_ordenada.nodo_cabeza) entonces {
          lista_ordenada.nodo_cabeza := nuevo_nodo;
        }
      }

      -- Eliminamos el elemento actual de la lista original.
      eliminar_nodo_del_final();
    }

    -- Actualizamos la cabeza y la cola de la lista original con los valores de la lista ordenada.
    nodo_cabeza := lista_ordenada.nodo_cabeza;
    nodo_cola := lista_ordenada.nodo_cola;
    tamaño := lista_ordenada.tamaño;
  }

  -- Método para imprimir la lista.
  procedure imprimir() {
    var nodo_actual: Nodo := nodo_cabeza;

    -- Recorremos la lista y mostramos cada elemento.
    mientras (nodo_actual no es nada) {
      mostrar(nodo_actual.elemento);
      mostrar(" ");

      nodo_actual := nodo_actual.siguiente;
    }

    mostrar("\n");
  }
}

-- Definición de la clase `Nodo`.
class Nodo {
  -- Atributos privados de la clase.
  private var elemento: entero;
  private var siguiente: Nodo o nada;

  -- Constructor de la clase.
  constructor crea_nuevo_nodo(elemento: entero) {
    this.elemento := elemento;
    this.siguiente := nada;
  }
}

-- Función principal del programa.
función main() {
  -- Creamos una nueva lista.
  var lista: Lista := crea_nueva_lista();

  -- Añadimos algunos elementos a la lista.
  lista.agregar_nodo_al_final(1);
  lista.agregar_nodo_al_final(3);
  lista.agregar_nodo_al_final(2);
  lista.agregar_nodo_al_final(5);
  lista.agregar_nodo_al_final(4);

  -- Mostramos la lista original.
  mostrar("Lista original: ");
  lista.imprimir();

  -- Ordenamos la lista.
  lista.ordenar();

  -- Mostramos la lista ordenada.
  mostrar("Lista ordenada: ");
  lista.imprimir();

  -- Buscamos un elemento en la lista.
  var elemento_a_buscar: entero := 3;
  si (lista.buscar_elemento(elemento_a_buscar)) entonces {
    mostrar("El elemento " + elemento_a_buscar + " se encontró en la lista.\n");
  } si no {
    mostrar("El elemento " + elemento_a_buscar + " no se encontró en la lista.\n");
  }

  -- Eliminamos el último nodo de la lista.
  lista.eliminar_nodo_del_final();

  -- Mostramos la lista después de eliminar el último nodo.
  mostrar("Lista después de eliminar el último nodo: ");
  lista.imprimir();
}

```

Explicación del código:

* Definimos dos clases: `Lista` y `Nodo`. La clase `Lista` representa una lista enlazada simple, mientras que la clase `Nodo` representa un nodo individual en la lista.
* En la clase `Lista`, definimos varios métodos para trabajar con la lista, incluyendo métodos para añadir y eliminar nodos, obtener el tamaño de la lista, obtener el elemento en un índice específico, buscar un elemento en la lista, ordenar la lista y imprimir la lista.
* En la clase `Nodo`, definimos un constructor para crear nuevos nodos y un método para obtener el elemento almacenado en el nodo.
* En la función `main`, creamos una nueva lista y añadimos algunos elementos a la lista.
* Mostramos la lista original, la ordenamos, buscamos un elemento en la lista, eliminamos el último nodo de la lista y mostramos la lista después de eliminar el último nodo.

El código anterior crea una lista enlazada simple en COOL, añade algunos elementos a la lista, la ordena, busca un elemento en la lista, elimina el último nodo de la lista y muestra la lista después de eliminar el último nodo. El código también explica claramente cada clase y método, lo que lo hace fácil de entender y mantener.