```cool
clase Calculadora {
    método suma(a: Número, b: Número): Número {
        a + b
    }

    método resta(a: Número, b: Número): Número {
        a - b
    }

    método multiplicación(a: Número, b: Número): Número {
        a * b
    }

    método división(a: Número, b: Número): Número {
        a / b
    }
}

clase CalculadoraAvanzada hereda de Calculadora {
    método potencia(a: Número, b: Número): Número {
        potencia(a, b)
    }

    método raízCuadrada(a: Número): Número {
        Math.sqrt(a)
    }

    método seno(a: Número): Número {
        Math.sin(a)
    }

    método coseno(a: Número): Número {
        Math.cos(a)
    }
}

clase Programa {
    método principal() {
        var calculadora = nueva Calculadora()

        var resultadoSuma = calculadora.suma(1, 2)
        escribir(resultadoSuma)

        var resultadoResta = calculadora.resta(5, 3)
        escribir(resultadoResta)

        var resultadoMultiplicación = calculadora.multiplicación(3, 4)
        escribir(resultadoMultiplicación)

        var resultadoDivisión = calculadora.división(10, 2)
        escribir(resultadoDivisión)

        var calculadoraAvanzada = nueva CalculadoraAvanzada()

        var resultadoPotencia = calculadoraAvanzada.potencia(2, 3)
        escribir(resultadoPotencia)

        var resultadoRaízCuadrada = calculadoraAvanzada.raízCuadrada(9)
        escribir(resultadoRaízCuadrada)

        var resultadoSeno = calculadoraAvanzada.seno(Math.PI / 2)
        escribir(resultadoSeno)

        var resultadoCoseno = calculadoraAvanzada.coseno(Math.PI / 4)
        escribir(resultadoCoseno)
    }
}

Programa.principal()
```

Este código es una calculadora que puede realizar operaciones básicas (suma, resta, multiplicación y división) y también operaciones avanzadas (potencia, raíz cuadrada, seno y coseno).

La clase `Calculadora` define los métodos para realizar las operaciones básicas, mientras que la clase `CalculadoraAvanzada` hereda de `Calculadora` y define los métodos para realizar las operaciones avanzadas.

La clase `Programa` define el método `principal`, que es el punto de entrada del programa. En este método se crean instancias de las clases `Calculadora` y `CalculadoraAvanzada`, y se utilizan para realizar las operaciones matemáticas.

El código está escrito en COOL, un lenguaje de programación orientado a objetos que es similar a Java.