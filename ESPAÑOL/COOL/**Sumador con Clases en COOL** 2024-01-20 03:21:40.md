```cool
clase Sumador {
    atributos:
        acumulador: entero
    métodos:
        inicializar: -> entero {
            acumulador <- 0
            return 0
        }

        sumar: -> entero {
            acumulador <- acumulador + 1
            return acumulador
        }
}

clase Main {
    métodos:
        main: -> entero {
            sumador <- new Sumador()
            sumador.inicializar()
            sumador.sumar()
            sumador.sumar()
            sumador.sumar()
            print(sumador.acumulador)
            return 0
        }
}
```

Este código es un ejemplo de un programa sencillo en COOL que implementa un sumador. El programa tiene dos clases: `Sumador` y `Main`. La clase `Sumador` representa el sumador en sí, mientras que la clase `Main` es el punto de entrada del programa.

La clase `Sumador` tiene dos atributos: `acumulador`, que almacena el valor actual del sumador, y `métodos`, que son las funciones que se pueden aplicar al sumador.

El método `inicializar` inicializa el acumulador a 0. El método `sumar` incrementa el acumulador en 1.

La clase `Main` tiene un único método: `main`, que es el punto de entrada del programa. El método `main` crea una instancia de la clase `Sumador`, llama al método `inicializar` para inicializar el acumulador, llama al método `sumar` tres veces para incrementar el acumulador, y finalmente imprime el valor del acumulador.

El programa imprimirá el número 3, que es el valor final del acumulador.

Este código es un ejemplo de un programa sencillo en COOL, pero muestra algunos de los conceptos básicos del lenguaje, como las clases, los atributos, los métodos y la herencia.