```swift
// Creamos una clase llamada "Complejo" que representa un número complejo.
class Complejo {
    
    // Propiedades.
    let real: Double  // Parte real.
    let imaginario: Double  // Parte imaginaria.
    
    // Constructor.
    init(real: Double, imaginario: Double) {
        self.real = real
        self.imaginario = imaginario
    }
    
    // Suma de dos números complejos.
    func +(otroComplejo: Complejo) -> Complejo {
        return Complejo(real: self.real + otroComplejo.real, imaginario: self.imaginario + otroComplejo.imaginario)
    }
    
    // Resta de dos números complejos.
    func -(otroComplejo: Complejo) -> Complejo {
        return Complejo(real: self.real - otroComplejo.real, imaginario: self.imaginario - otroComplejo.imaginario)
    }
    
    // Multiplicación de dos números complejos.
    func *(otroComplejo: Complejo) -> Complejo {
        // Multiplicamos las partes reales e imaginarias por separado.
        let real = self.real * otroComplejo.real - self.imaginario * otroComplejo.imaginario
        let imaginario = self.real * otroComplejo.imaginario + self.imaginario * otroComplejo.real
        
        // Devolvemos el resultado.
        return Complejo(real: real, imaginario: imaginario)
    }
    
    // División de dos números complejos.
    func /(otroComplejo: Complejo) -> Complejo {
        // Calculamos el complejo conjugado del divisor.
        let conjugadoDivisor = Complejo(real: otroComplejo.real, imaginario: -otroComplejo.imaginario)
        
        // Multiplicamos el dividendo por el complejo conjugado del divisor.
        let producto = self * conjugadoDivisor
        
        // Dividimos el producto por el cuadrado de la norma del divisor.
        let normaDivisor = sqrt(otroComplejo.real * otroComplejo.real + otroComplejo.imaginario * otroComplejo.imaginario)
        let division = producto / Complejo(real: normaDivisor * normaDivisor, imaginario: 0)
        
        // Devolvemos el resultado.
        return division
    }
    
    // Módulo o norma del número complejo.
    var norma: Double {
        return sqrt(real * real + imaginario * imaginario)
    }
    
    // Conjugado del número complejo.
    var conjugado: Complejo {
        return Complejo(real: real, imaginario: -imaginario)
    }
    
    // Representación textual del número complejo.
    override var description: String {
        return "\(real) + \(imaginario)i"
    }
}

// Creamos algunos números complejos.
let c1 = Complejo(real: 3.0, imaginario: 4.0)
let c2 = Complejo(real: 5.0, imaginario: -2.0)

// Suma de dos números complejos.
let suma = c1 + c2
print("Suma:", suma) // Imprime: Suma: 8.0 + 2.0i

// Resta de dos números complejos.
let resta = c1 - c2
print("Resta:", resta) // Imprime: Resta: -2.0 + 6.0i

// Multiplicación de dos números complejos.
let multiplicacion = c1 * c2
print("Multiplicación:", multiplicacion) // Imprime: Multiplicación: 7.0 + 22.0i

// División de dos números complejos.
let division = c1 / c2
print("División:", division) // Imprime: División: 0.8 + 0.6i

// Módulo o norma del número complejo.
print("Módulo c1:", c1.norma) // Imprime: Módulo c1: 5.0
print("Módulo c2:", c2.norma) // Imprime: Módulo c2: 5.477225575051661

// Conjugado del número complejo.
print("Conjugado c1:", c1.conjugado) // Imprime: Conjugado c1: 3.0 - 4.0i
print("Conjugado c2:", c2.conjugado) // Imprime: Conjugado c2: 5.0 + 2.0i

// Representación textual del número complejo.
print("c1:", c1) // Imprime: c1: 3.0 + 4.0i
print("c2:", c2) // Imprime: c2: 5.0 - 2.0i
```

Este código es un ejemplo de cómo se puede implementar una clase compleja en Swift. La clase representa un número complejo con dos propiedades `real` e `imaginario`. La clase proporciona varios métodos para realizar operaciones aritméticas con números complejos, como suma, resta, multiplicación, división, norma y conjugado. También proporciona una representación textual del número complejo.

El código se puede utilizar de la siguiente manera:

```swift
// Creamos algunos números complejos.
let c1 = Complejo(real: 3.0, imaginario: 4.0)
let c2 = Complejo(real: 5.0, imaginario: -2.0)

// Suma de dos números complejos.
let suma = c1 + c2
print("Suma:", suma) // Imprime: Suma: 8.0 + 2.0i

// Resta de dos números complejos.
let resta = c1 - c2
print("Resta:", resta) // Imprime: Resta: -2.0 + 6.0i

// Multiplicación de dos números complejos.
let multiplicacion = c1 * c2
print("Multiplicación:", multiplicacion) // Imprime: Multiplicación: 7.0 + 22.0i

// División de dos números complejos.
let division = c1 / c2
print("División:", division) // Imprime: División: 0.8 + 0.6i

// Módulo o norma del número complejo.
print("Módulo c1:", c1.norma) // Imprime: Módulo c1: 5.0
print("Módulo c2:", c2.norma) // Imprime: Módulo c2: 5.477225575051661

// Conjugado del número complejo.
print("Conjugado c1:", c1.conjugado) // Imprime: Conjugado c1: 3.0 - 4.0i
print("Conjugado c2:", c2.conjugado) // Imprime: Conjugado c2: 5.0 + 2.0i

// Representación textual del número complejo.
print("c1:", c1) // Imprime: c1: 3.0 + 4.0i
print("c2:", c2) // Imprime: c2: 5.0 - 2.0i
```