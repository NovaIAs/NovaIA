```swift
// Importación de las bibliotecas necesarias
import Foundation
import UIKit

// Definición de la clase principal
class CalculadoraCompleja {

    // Propiedades de la clase
    private var numero1: Double
    private var numero2: Double
    private var operacion: String

    // Inicializador de la clase
    init(numero1: Double, numero2: Double, operacion: String) {
        self.numero1 = numero1
        self.numero2 = numero2
        self.operacion = operacion
    }

    // Función para realizar la operación seleccionada
    func calcular() -> Double {

        // Comprobación de la operación seleccionada
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

// Definición de la función principal
func main() {

    // Creación de una instancia de la clase CalculadoraCompleja
    let calculadora = CalculadoraCompleja(numero1: 10.0, numero2: 5.0, operacion: "+")

    // Llamada a la función calcular() para realizar la operación
    let resultado = calculadora.calcular()

    // Impresión del resultado en la consola
    print("El resultado de la operación es \(resultado)")

}

// Llamada a la función principal para iniciar el programa
main()
```

Explicación del código:

- Se importan las bibliotecas necesarias para el funcionamiento del programa.

- Se define la clase CalculadoraCompleja que contiene las propiedades necesarias para realizar las operaciones matemáticas.

- Se define el inicializador de la clase CalculadoraCompleja que recibe los dos números a operar y la operación a realizar.

- Se define la función calcular() que realiza la operación matemática seleccionada y retorna el resultado.

- Se define la función principal main() que crea una instancia de la clase CalculadoraCompleja, llama a la función calcular() para realizar la operación y imprime el resultado en la consola.

- Se llama a la función main() para iniciar el programa.