```cool
clase Lista {

    // Atributos
    lista: List;
    longitud: Int;

    // Constructor
    constructor(lista_init: List, longitud_init: Int) {
        lista := lista_init;
        longitud := longitud_init;
    }

    // Métodos
    agregar(elemento: Any) {
        // Agrega un elemento al final de la lista
        self.lista := self.lista + [elemento];
        self.longitud := self.longitud + 1;
    }

    eliminar(indice: Int) {
        // Elimina el elemento en la posición indicada
        self.lista := self.lista[:indice] + self.lista[indice+1:];
        self.longitud := self.longitud - 1;
    }

    obtener(indice: Int) : Any {
        // Obtiene el elemento en la posición indicada
        return self.lista[indice];
    }

    esta_vacia() : Bool {
        // Devuelve verdadero si la lista está vacía
        return self.longitud == 0;
    }

    imprimir() {
        // Imprime la lista en la consola
        for (elemento in self.lista) {
            System.out.print(elemento + " ");
        }
        System.out.println();
    }
}

clase Pila {

    // Atributos
    pila: List;
    longitud: Int;

    // Constructor
    constructor(pila_init: List, longitud_init: Int) {
        pila := pila_init;
        longitud := longitud_init;
    }

    // Métodos
    apilar(elemento: Any) {
        // Agrega un elemento al final de la pila
        self.pila := self.pila + [elemento];
        self.longitud := self.longitud + 1;
    }

    desapilar() : Any {
        // Elimina y devuelve el elemento en el tope de la pila
        elemento := self.pila[self.longitud - 1];
        self.pila := self.pila[:self.longitud - 1];
        self.longitud := self.longitud - 1;
        return elemento;
    }

    obtener_tope() : Any {
        // Obtiene el elemento en el tope de la pila
        return self.pila[self.longitud - 1];
    }

    esta_vacia() : Bool {
        // Devuelve verdadero si la pila está vacía
        return self.longitud == 0;
    }

    imprimir() {
        // Imprime la pila en la consola
        for (elemento in self.pila) {
            System.out.print(elemento + " ");
        }
        System.out.println();
    }
}

clase Cola {

    // Atributos
    cola: List;
    longitud: Int;

    // Constructor
    constructor(cola_init: List, longitud_init: Int) {
        cola := cola_init;
        longitud := longitud_init;
    }

    // Métodos
    encolar(elemento: Any) {
        // Agrega un elemento al final de la cola
        self.cola := self.cola + [elemento];
        self.longitud := self.longitud + 1;
    }

    desencolar() : Any {
        // Elimina y devuelve el elemento al frente de la cola
        elemento := self.cola[0];
        self.cola := self.cola[1:];
        self.longitud := self.longitud - 1;
        return elemento;
    }

    obtener_frente() : Any {
        // Obtiene el elemento al frente de la cola
        return self.cola[0];
    }

    esta_vacia() : Bool {
        // Devuelve verdadero si la cola está vacía
        return self.longitud == 0;
    }

    imprimir() {
        // Imprime la cola en la consola
        for (elemento in self.cola) {
            System.out.print(elemento + " ");
        }
        System.out.println();
    }
}

clase Arbol {

    // Atributos
    valor: Any;
    hijos: List;

    // Constructor
    constructor(valor_init: Any, hijos_init: List) {
        valor := valor_init;
        hijos := hijos_init;
    }

    // Métodos
    agregar_hijo(hijo: Arbol) {
        // Agrega un hijo al árbol
        self.hijos := self.hijos + [hijo];
    }

    eliminar_hijo(indice: Int) {
        // Elimina el hijo en la posición indicada
        self.hijos := self.hijos[:indice] + self.hijos[indice+1:];
    }

    obtener_hijo(indice: Int) : Arbol {
        // Obtiene el hijo en la posición indicada
        return self.hijos[indice];
    }

    esta_vacio() : Bool {
        // Devuelve verdadero si el árbol está vacío
        return self.hijos.es_vac()