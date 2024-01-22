```swift

import UIKit

class ComplejoViewController: UIViewController {

    override func viewDidLoad() {
        super.viewDidLoad()

        // Crear una matriz de números complejos
        var numerosComplejos: [Complejo] = []

        // Añadir algunos números complejos a la matriz
        numerosComplejos.append(Complejo(parteReal: 1, parteImaginaria: 2))
        numerosComplejos.append(Complejo(parteReal: 3, parteImaginaria: 4))
        numerosComplejos.append(Complejo(parteReal: 5, parteImaginaria: 6))

        // Sumar los números complejos de la matriz
        var suma = Complejo(parteReal: 0, parteImaginaria: 0)
        for numero in numerosComplejos {
            suma = suma + numero
        }

        // Mostrar la suma de los números complejos
        print("La suma de los números complejos es \(suma)")

        // Restar los números complejos de la matriz
        var resta = Complejo(parteReal: 0, parteImaginaria: 0)
        for numero in numerosComplejos {
            resta = resta - numero
        }

        // Mostrar la resta de los números complejos
        print("La resta de los números complejos es \(resta)")

        // Multiplicar los números complejos de la matriz
        var producto = Complejo(parteReal: 1, parteImaginaria: 0)
        for numero in numerosComplejos {
            producto = producto * numero
        }

        // Mostrar el producto de los números complejos
        print("El producto de los números complejos es \(producto)")

        // Dividir los números complejos de la matriz
        var division = Complejo(parteReal: 1, parteImaginaria: 0)
        for numero in numerosComplejos {
            division = division / numero
        }

        // Mostrar la división de los números complejos
        print("La división de los números complejos es \(division)")
    }
}

struct Complejo {

    var parteReal: Double
    var parteImaginaria: Double

    init(parteReal: Double, parteImaginaria: Double) {
        self.parteReal = parteReal
        self.parteImaginaria = parteImaginaria
    }

    static func +(lhs: Complejo, rhs: Complejo) -> Complejo {
        return Complejo(parteReal: lhs.parteReal + rhs.parteReal, parteImaginaria: lhs.parteImaginaria + rhs.parteImaginaria)
    }

    static func -(lhs: Complejo, rhs: Complejo) -> Complejo {
        return Complejo(parteReal: lhs.parteReal - rhs.parteReal, parteImaginaria: lhs.parteImaginaria - rhs.parteImaginaria)
    }

    static func *(lhs: Complejo, rhs: Complejo) -> Complejo {
        return Complejo(parteReal: lhs.parteReal * rhs.parteReal - lhs.parteImaginaria * rhs.parteImaginaria, parteImaginaria: lhs.parteReal * rhs.parteImaginaria + lhs.parteImaginaria * rhs.parteReal)
    }

    static func /(lhs: Complejo, rhs: Complejo) -> Complejo {
        let denominador = rhs.parteReal * rhs.parteReal + rhs.parteImaginaria * rhs.parteImaginaria
        return Complejo(parteReal: (lhs.parteReal * rhs.parteReal + lhs.parteImaginaria * rhs.parteImaginaria) / denominador, parteImaginaria: (lhs.parteImaginaria * rhs.parteReal - lhs.parteReal * rhs.parteImaginaria) / denominador)
    }
}

```

Explicación del código:

1. Definimos una clase `ComplejoViewController` que extiende `UIViewController`. Esta clase es la clase principal de nuestro controlador de vista.
2. Sobrescribimos el método `viewDidLoad()` para que se ejecute cuando se cargue la vista.
3. Creamos una matriz de números complejos llamada `numerosComplejos`.
4. Añadimos algunos números complejos a la matriz.
5. Sumamos los números complejos de la matriz.
6. Restamos los números complejos de la matriz.
7. Multiplicamos los números complejos de la matriz.
8. Dividimos los números complejos de la matriz.
9. Definimos una estructura `Complejo` que representa un número complejo.
10. Definimos los operadores `+`, `-`, `*` y `/` para la estructura `Complejo`. Estos operadores se utilizan para sumar, restar, multiplicar y dividir números complejos.
11. Instanciamos un objeto `ComplejoViewController` y lo mostramos en la pantalla.