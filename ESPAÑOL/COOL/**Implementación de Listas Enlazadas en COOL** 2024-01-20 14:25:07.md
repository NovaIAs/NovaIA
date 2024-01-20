```cool
-- Definición de Clases
clase Nodo {
  atributos:
    valor: entero;
    siguiente: Nodo;
};

-- Definición de Métodos
clase Lista {
  atributos:
    primero: Nodo;
    último: Nodo;

  métodos:
    -- Constructor de la lista
    iniciar() {
      self.primero := Ninguno;
      self.último := Ninguno;
    }

    -- Agrega un nuevo elemento al final de la lista
    agregar(valor: entero) {
      nuevo_nodo := new Nodo;
      nuevo_nodo.valor := valor;
      nuevo_nodo.siguiente := Ninguno;

      si self.primero = Ninguno entonces {
        self.primero := nuevo_nodo;
      } si no {
        self.último.siguiente := nuevo_nodo;
      } fi;

      self.último := nuevo_nodo;
    }

    -- Elimina el primer elemento de la lista
    eliminar_primero() {
      si self.primero = Ninguno entonces {
        error("La lista está vacía.");
      } si no {
        primero_actual := self.primero;

        si self.primero = self.último entonces {
          self.primero := Ninguno;
          self.último := Ninguno;
        } si no {
          self.primero := self.primero.siguiente;
        } fi;

        delete primero_actual;
      } fi;
    }

    -- Busca un elemento en la lista y devuelve su posición
    buscar(valor: entero): entero {
      posicion := 0;
      nodo_actual := self.primero;

      mientras nodo_actual /= Ninguno haz {
        si nodo_actual.valor = valor entonces {
          devolver posicion;
        } fi;

        nodo_actual := nodo_actual.siguiente;
        posicion := posicion + 1;
      } od;

      devolver -1;
    }

    -- Imprime la lista
    imprimir() {
      nodo_actual := self.primero;

      mientras nodo_actual /= Ninguno haz {
        escribir(nodo_actual.valor);
        escribir(" ");

        nodo_actual := nodo_actual.siguiente;
      } od;

      escribir("");
    }
};

-- Bloque Principal
principal {
  lista := new Lista;
  lista.iniciar();

  lista.agregar(1);
  lista.agregar(2);
  lista.agregar(3);
  lista.agregar(4);
  lista.agregar(5);

  lista.imprimir();

  lista.eliminar_primero();

  lista.imprimir();

  posicion := lista.buscar(3);

  si posicion = -1 entonces {
    escribir("El elemento no se encontró en la lista.");
  } si no {
    escribir("El elemento se encontró en la posición ", posicion);
  } fi;
}
```

Explicación:

El código crea una clase llamada `Nodo` para representar los nodos de una lista enlazada. Cada nodo tiene un atributo `valor` que almacena el valor del nodo y un atributo `siguiente` que apunta al siguiente nodo en la lista.

También crea una clase llamada `Lista` para representar una lista enlazada. La clase `Lista` tiene un atributo `primero` que apunta al primer nodo de la lista y un atributo `último` que apunta al último nodo de la lista.

La clase `Lista` tiene varios métodos, incluyendo un método `iniciar()` para inicializar la lista, un método `agregar()` para agregar un nuevo elemento al final de la lista, un método `eliminar_primero()` para eliminar el primer elemento de la lista, un método `buscar()` para buscar un elemento en la lista y devolver su posición, y un método `imprimir()` para imprimir la lista.

El bloque principal crea una instancia de la clase `Lista` y llama al método `iniciar()` para inicializar la lista. Luego agrega varios elementos a la lista usando el método `agregar()`.

A continuación, imprime la lista usando el método `imprimir()`, elimina el primer elemento de la lista usando el método `eliminar_primero()`, vuelve a imprimir la lista y busca un elemento en la lista usando el método `buscar()`.

El código es complejo pero demuestra varias características del lenguaje COOL, incluyendo la creación de clases, la herencia, los métodos y las listas enlazadas.