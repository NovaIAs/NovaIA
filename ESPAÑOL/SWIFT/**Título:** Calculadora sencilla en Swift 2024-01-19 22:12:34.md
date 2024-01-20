```swift
// Importamos las librerías necesarias
import Foundation
import UIKit

// Definimos una clase llamada "Calculadora"
class Calculadora {

    // Declaramos las propiedades de la clase
    var numero1: Double
    var numero2: Double
    var operacion: String

    // Inicializamos la clase con los valores de las propiedades
    init(numero1: Double, numero2: Double, operacion: String) {
        self.numero1 = numero1
        self.numero2 = numero2
        self.operacion = operacion
    }

    // Definimos un método llamado "calcular" que realiza la operación especificada
    func calcular() -> Double {
        switch operacion {
            case "+":
                return numero1 + numero2
            case "-":
                return numero1 - numero2
            case "*":
                return numero1 * numero2
            case "/":
                return numero1 / numero2
            default:
                return 0.0
        }
    }
}

// Creamos una instancia de la clase "Calculadora"
let calculadora = Calculadora(numero1: 10.0, numero2: 5.0, operacion: "+")

// Llamamos al método "calcular" para obtener el resultado de la operación
let resultado = calculadora.calcular()

// Imprimimos el resultado en la consola
print("El resultado de la operación es: \(resultado)")
```

**Explicación del código:**

1. Importamos las librerías necesarias.

2. Definimos una clase llamada "Calculadora" que tiene tres propiedades: "numero1", "numero2" y "operacion".

3. Inicializamos la clase con los valores de las propiedades.

4. Definimos un método llamado "calcular" que realiza la operación especificada.

5. Creamos una instancia de la clase "Calculadora".

6. Llamamos al método "calcular" para obtener el resultado de la operación.

7. Imprimimos el resultado en la consola.