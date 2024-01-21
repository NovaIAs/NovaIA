```
(*
* El siguiente código es un ejemplo de un programa complejo en el lenguaje de programación COOL.
* El programa implementa un árbol binario de búsqueda y permite insertar, buscar y eliminar elementos.
* El árbol se implementa utilizando una lista enlazada, donde cada nodo contiene un elemento y un puntero al nodo izquierdo y derecho.
*)

(*
* Definición de los tipos de datos.
*)

type Nodo {
  elemento: Int;
  izquierda: Nodo?;
  derecha: Nodo?;
}

(*
* Definición de las funciones.
*)

función insertar(árbol: Nodo?; elemento: Int): Nodo? {
  si árbol es vacío {
    devolver nuevo Nodo(elemento, null, null);
  } si elemento < árbol.elemento {
    árbol.izquierda = insertar(árbol.izquierda, elemento);
  } si no {
    árbol.derecha = insertar(árbol.derecha, elemento);
  }
  devolver árbol;
}

función buscar(árbol: Nodo?; elemento: Int): Nodo? {
  si árbol es vacío {
    devolver null;
  } si elemento = árbol.elemento {
    devolver árbol;
  } si elemento < árbol.elemento {
    devolver buscar(árbol.izquierda, elemento);
  } si no {
    devolver buscar(árbol.derecha, elemento);
  }
}

función eliminar(árbol: Nodo?; elemento: Int): Nodo? {
  si árbol es vacío {
    devolver null;
  } si elemento < árbol.elemento {
    árbol.izquierda = eliminar(árbol.izquierda, elemento);
  } si no si elemento > árbol.elemento {
    árbol.derecha = eliminar(árbol.derecha, elemento);
  } si no {
    si árbol.izquierda es vacío {
      devolver árbol.derecha;
    } si no si árbol.derecha es vacío {
      devolver árbol.izquierda;
    } si no {
      árbol.elemento = encontrarMínimo(árbol.derecha).elemento;
      árbol.derecha = eliminar(árbol.derecha, árbol.elemento);
    }
  }
  devolver árbol;
}

función encontrarMínimo(árbol: Nodo?): Nodo? {
  si árbol.izquierda es vacío {
    devolver árbol;
  } si no {
    devolver encontrarMínimo(árbol.izquierda);
  }
}

(*
* Uso de las funciones.
*)

var árbol: Nodo? = null;
árbol = insertar(árbol, 10);
árbol = insertar(árbol, 5);
árbol = insertar(árbol, 15);
árbol = insertar(árbol, 2);
árbol = insertar(árbol, 7);
árbol = insertar(árbol, 12);
árbol = insertar(árbol, 20);

var elemento = buscar(árbol, 15);
si elemento es null {
  imprimir("El elemento no se encontró.");
} si no {
  imprimir("El elemento se encontró.");
}

árbol = eliminar(árbol, 15);

elemento = buscar(árbol, 15);
si elemento es null {
  imprimir("El elemento se eliminó correctamente.");
} si no {
  imprimir("El elemento no se eliminó correctamente.");
}
```

(*
* Explicación del código.
*)

* La primera parte del código define los tipos de datos que se utilizarán en el programa. En este caso, se define un tipo de dato llamado `Nodo` que representa un nodo del árbol binario de búsqueda. Un nodo contiene un elemento y dos punteros, uno al nodo izquierdo y otro al nodo derecho.
* La segunda parte del código define las funciones que se utilizarán para manipular el árbol binario de búsqueda. Las funciones incluyen:
    * `insertar`: Esta función inserta un nuevo elemento en el árbol binario de búsqueda.
    * `buscar`: Esta función busca un elemento en el árbol binario de búsqueda.
    * `eliminar`: Esta función elimina un elemento del árbol binario de búsqueda.
    * `encontrarMínimo`: Esta función encuentra el nodo con el elemento mínimo en el árbol binario de búsqueda.
* La tercera parte del código muestra cómo utilizar las funciones para manipular el árbol binario de búsqueda. En este caso, se crea un árbol binario de búsqueda, se insertan varios elementos en el árbol, se busca un elemento en el árbol y se elimina un elemento del árbol.