```cool
característica Contador {
    inicializar(valorInicial: Entero) {
        // Inicializa el contador con el valor inicial proporcionado
        _valor := valorInicial
    }

    incrementar() {
        // Incrementa el valor del contador en 1
        _valor := _valor + 1
    }

    decrementar() {
        // Decrementa el valor del contador en 1
        _valor := _valor - 1
    }

    obtenerValor(): Entero {
        // Devuelve el valor actual del contador
        return _valor
    }

    _valor: Entero  // El valor actual del contador
}

característica Programa {
    main() {
        // Crea un objeto Contador e inicialízalo con el valor 10
        contador := Contador.new(10)

        // Imprime el valor inicial del contador
        Console.imprimir("Valor inicial del contador: {contador.obtenerValor()}")

        // Incrementa el valor del contador en 5
        contador.incrementar(5)

        // Imprime el valor incrementado del contador
        Console.imprimir("Valor incrementado del contador: {contador.obtenerValor()}")

        // Decrementa el valor del contador en 3
        contador.decrementar(3)

        // Imprime el valor decrementado del contador
        Console.imprimir("Valor decrementado del contador: {contador.obtenerValor()}")
    }

    contador: Contador  // El objeto Contador que se utiliza en el programa
}
```

Explicación del código:

1. **Clase `Contador`:**

    * Esta clase define un tipo de datos abstracto para representar un contador.
    * Proporciona métodos para inicializar el contador con un valor inicial, incrementar el valor del contador, decrementar el valor del contador y obtener el valor actual del contador.

2. **Clase `Programa`:**

    * Esta clase define una clase de programa simple que crea un objeto `Contador` y demuestra cómo utilizar los métodos proporcionados por la clase `Contador`.

3. **Método `main`:**

    * Este método es el punto de entrada al programa.
    * Crea un objeto `Contador` e lo inicializa con el valor 10.
    * Incrementa el valor del contador en 5.
    * Decrementa el valor del contador en 3.
    * Imprime el valor inicial, incrementado y decrementado del contador en la consola.

Este código demuestra cómo definir y utilizar una clase en COOL para representar un contador. El código también demuestra cómo crear objetos de una clase y utilizar los métodos proporcionados por la clase.