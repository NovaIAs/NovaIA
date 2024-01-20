```swift
// Importamos las librerías necesarias
import Foundation
import UIKit

// Definimos una clase llamada "Complejo" que representará los números complejos
class Complejo {
    // Propiedades de la clase
    var real: Double
    var imaginario: Double

    // Inicializador de la clase
    init(real: Double, imaginario: Double) {
        self.real = real
        self.imaginario = imaginario
    }

    // Función para sumar dos números complejos
    func sumar(otroComplejo: Complejo) -> Complejo {
        // Creamos un nuevo objeto Complejo con el resultado de la suma
        let resultado = Complejo(real: self.real + otroComplejo.real, imaginario: self.imaginario + otroComplejo.imaginario)

        // Devolvemos el resultado
        return resultado
    }

    // Función para restar dos números complejos
    func restar(otroComplejo: Complejo) -> Complejo {
        // Creamos un nuevo objeto Complejo con el resultado de la resta
        let resultado = Complejo(real: self.real - otroComplejo.real, imaginario: self.imaginario - otroComplejo.imaginario)

        // Devolvemos el resultado
        return resultado
    }

    // Función para multiplicar dos números complejos
    func multiplicar(otroComplejo: Complejo) -> Complejo {
        // Creamos un nuevo objeto Complejo con el resultado de la multiplicación
        let resultado = Complejo(real: self.real * otroComplejo.real - self.imaginario * otroComplejo.imaginario, imaginario: self.real * otroComplejo.imaginario + self.imaginario * otroComplejo.real)

        // Devolvemos el resultado
        return resultado
    }

    // Función para dividir dos números complejos
    func dividir(otroComplejo: Complejo) -> Complejo {
        // Comprobamos que el divisor no sea cero
        guard otroComplejo.real != 0 || otroComplejo.imaginario != 0 else {
            fatalError("No se puede dividir por cero")
        }

        // Creamos un nuevo objeto Complejo con el resultado de la división
        let resultado = Complejo(real: (self.real * otroComplejo.real + self.imaginario * otroComplejo.imaginario) / (otroComplejo.real * otroComplejo.real + otroComplejo.imaginario * otroComplejo.imaginario), imaginario: (self.imaginario * otroComplejo.real - self.real * otroComplejo.imaginario) / (otroComplejo.real * otroComplejo.real + otroComplejo.imaginario * otroComplejo.imaginario))

        // Devolvemos el resultado
        return resultado
    }
}

// Creamos dos objetos Complejo
let c1 = Complejo(real: 3, imaginario: 4)
let c2 = Complejo(real: 5, imaginario: -2)

// Sumamos los dos objetos Complejo
let resultadoSuma = c1.sumar(otroComplejo: c2)

// Restamos los dos objetos Complejo
let resultadoResta = c1.restar(otroComplejo: c2)

// Multiplicamos los dos objetos Complejo
let resultadoMultiplicacion = c1.multiplicar(otroComplejo: c2)

// Dividimos los dos objetos Complejo
let resultadoDivision = c1.dividir(otroComplejo: c2)

// Imprimimos los resultados
print("Resultado de la suma:", resultadoSuma)
print("Resultado de la resta:", resultadoResta)
print("Resultado de la multiplicación:", resultadoMultiplicacion)
print("Resultado de la división:", resultadoDivision)
```

Explicación del código:

* Definimos una clase llamada "Complejo" que representa los números complejos.
* La clase "Complejo" tiene dos propiedades: "real" e "imaginario".
* La clase "Complejo" tiene un inicializador que toma dos parámetros: "real" e "imaginario".
* La clase "Complejo" tiene cuatro funciones: "sumar", "restar", "multiplicar" y "dividir". Estas funciones toman un objeto Complejo como parámetro y devuelven un nuevo objeto Complejo con el resultado de la operación.
* Creamos dos objetos Complejo llamados "c1" y "c2".
* Sumamos los dos objetos Complejo usando la función "sumar".
* Restamos los dos objetos Complejo usando la función "restar".
* Multiplicamos los dos objetos Complejo usando la función "multiplicar".
* Dividimos los dos objetos Complejo usando la función "dividir".
* Imprimimos los resultados de las operaciones.