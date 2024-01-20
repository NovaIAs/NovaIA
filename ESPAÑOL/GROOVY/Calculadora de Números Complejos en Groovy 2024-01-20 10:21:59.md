```groovy
// Nombre: CalculadoraCompleja.groovy
// Descripción: Una calculadora compleja en Groovy que permite realizar operaciones complejas con números complejos.

// Definición de la clase NúmeroComplejo
class NumeroComplejo {
    // Atributos
    BigDecimal real
    BigDecimal imaginario

    // Constructor
    NumeroComplejo(BigDecimal real, BigDecimal imaginario) {
        this.real = real
        this.imaginario = imaginario
    }

    // Métodos
    NumeroComplejo sumar(NumeroComplejo otroNumero) {
        new NumeroComplejo(this.real + otroNumero.real, this.imaginario + otroNumero.imaginario)
    }

    NumeroComplejo restar(NumeroComplejo otroNumero) {
        new NumeroComplejo(this.real - otroNumero.real, this.imaginario - otroNumero.imaginario)
    }

    NumeroComplejo multiplicar(NumeroComplejo otroNumero) {
        new NumeroComplejo(
                (this.real * otroNumero.real) - (this.imaginario * otroNumero.imaginario),
                (this.real * otroNumero.imaginario) + (this.imaginario * otroNumero.real)
        )
    }

    NumeroComplejo dividir(NumeroComplejo otroNumero) {
        BigDecimal denominador = (otroNumero.real * otroNumero.real) + (otroNumero.imaginario * otroNumero.imaginario)
        new NumeroComplejo(
                ((this.real * otroNumero.real) + (this.imaginario * otroNumero.imaginario)) / denominador,
                ((this.imaginario * otroNumero.real) - (this.real * otroNumero.imaginario)) / denominador
        )
    }

    String toString() {
        "$real + $imaginario i"
    }
}

// Definición de la clase CalculadoraCompleja
class CalculadoraCompleja {

    // Método principal
    static void main(String[] args) {
        // Creamos dos números complejos
        NumeroComplejo numero1 = new NumeroComplejo(3, 4)
        NumeroComplejo numero2 = new NumeroComplejo(5, -2)

        // Realizamos operaciones con los números complejos
        NumeroComplejo suma = numero1.sumar(numero2)
        NumeroComplejo resta = numero1.restar(numero2)
        NumeroComplejo multiplicacion = numero1.multiplicar(numero2)
        NumeroComplejo division = numero1.dividir(numero2)

        // Imprimimos los resultados
        println("Suma: $suma")
        println("Resta: $resta")
        println("Multiplicación: $multiplicacion")
        println("División: $division")
    }
}
```

Explicación del código:

* La clase NúmeroComplejo representa un número complejo. Tiene dos atributos, real e imaginario, que son de tipo BigDecimal.
* La clase CalculadoraCompleja contiene el método principal, que es el punto de entrada del programa.
* En el método principal, se crean dos objetos de tipo NúmeroComplejo, numero1 y numero2.
* Se realizan las operaciones de suma, resta, multiplicación y división con los números complejos.
* Se imprimen los resultados de las operaciones.

Este código es un ejemplo de cómo crear y utilizar una clase en Groovy. También muestra cómo realizar operaciones con números complejos.