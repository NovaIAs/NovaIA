```cool
clase nodo {
    datos: Tipo;
    siguiente: nodo @ ninguno;
}

clase listaEnlazada {
    primero: nodo @ ninguno;
    último: nodo @ ninguno;

    agregar(valor: Tipo) {
        nuevoNodo := nuevo nodo {
            datos: valor,
            siguiente: ninguno
        };

        si primero = ninguno entonces
            primero := nuevoNodo;
            último := nuevoNodo;
        si no
            último.siguiente := nuevoNodo;
            último := nuevoNodo;
        fin si
    }

    obtener(índice: Entero): Tipo {
        actual := primero;
        para i := 0 hasta índice-1 hacer
            actual := actual.siguiente;
        fin para
        devolver actual.datos
    }

    establecer(índice: Entero, valor: Tipo) {
        actual := primero;
        para i := 0 hasta índice-1 hacer
            actual := actual.siguiente;
        fin para
        actual.datos := valor
    }

    insertar(índice: Entero, valor: Tipo) {
        si índice = 0 entonces
            agregar(valor);
        si no
            previo := primero;
            para i := 0 hasta índice-2 hacer
                previo := previo.siguiente;
            fin para
            nuevoNodo := nuevo nodo {
                datos: valor,
                siguiente: previo.siguiente
            };
            previo.siguiente := nuevoNodo;
        fin si
    }

    eliminar(índice: Entero) {
        si índice = 0 entonces
            primero := primero.siguiente;
        si no
            previo := primero;
            para i := 0 hasta índice-2 hacer
                previo := previo.siguiente;
            fin para
            previo.siguiente := previo.siguiente.siguiente;
        fin si
    }

    recorrer() {
        actual := primero;
        mientras actual != ninguno hacer
            escribir(actual.datos, " ");
            actual := actual.siguiente;
        fin mientras
    }
}

clase ProgramaPrincipal {
    principal() {
        lista := nueva listaEnlazada;
        lista.agregar(10);
        lista.agregar(20);
        lista.agregar(30);

        lista.obtener(1);  -- devuelve 20
        lista.establecer(1, 50);
        lista.insertar(1, 40);
        lista.eliminar(1);
        lista.recorrer();  -- imprime "10 50 30"
    }
}
```

Explicación del código:

* La clase `nodo` representa un nodo de una lista enlazada. Contiene un campo `datos` que almacena el valor del nodo y un campo `siguiente` que apunta al siguiente nodo de la lista o es `ninguno` si es el último nodo.
* La clase `listaEnlazada` representa una lista enlazada. Contiene un campo `primero` que apunta al primer nodo de la lista y un campo `último` que apunta al último nodo de la lista. También contiene varios métodos para agregar, obtener, establecer, insertar y eliminar elementos de la lista, así como un método para recorrer la lista e imprimir sus elementos.
* La clase `ProgramaPrincipal` contiene el método `principal()` que se ejecuta al iniciar el programa. En este método se crea una instancia de la clase `listaEnlazada`, se agregan algunos elementos a la lista y luego se utilizan los métodos de la clase `listaEnlazada` para obtener, establecer, insertar y eliminar elementos de la lista. Finalmente, se utiliza el método `recorrer()` para imprimir los elementos de la lista.