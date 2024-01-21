```swift
// Importación de las bibliotecas necesarias
import Foundation
import UIKit

// Definición de un protocolo para representar una figura geométrica
protocol FiguraGeométrica {
    var nombre: String { get }
    var área: Double { get }
    var perímetro: Double { get }
}

// Implementación de la clase Círculo que implementa el protocolo FiguraGeométrica
class Círculo: FiguraGeométrica {
    var nombre: String = "Círculo"
    var radio: Double
    
    init(radio: Double) {
        self.radio = radio
    }
    
    var área: Double {
        return Double.pi * radio * radio
    }
    
    var perímetro: Double {
        return 2 * Double.pi * radio
    }
}

// Implementación de la clase Rectángulo que implementa el protocolo FiguraGeométrica
class Rectángulo: FiguraGeométrica {
    var nombre: String = "Rectángulo"
    var ancho: Double
    var largo: Double
    
    init(ancho: Double, largo: Double) {
        self.ancho = ancho
        self.largo = largo
    }
    
    var área: Double {
        return ancho * largo
    }
    
    var perímetro: Double {
        return 2 * (ancho + largo)
    }
}

// Implementación de la clase Triángulo que implementa el protocolo FiguraGeométrica
class Triángulo: FiguraGeométrica {
    var nombre: String = "Triángulo"
    var base: Double
    var altura: Double
    
    init(base: Double, altura: Double) {
        self.base = base
        self.altura = altura
    }
    
    var área: Double {
        return 0.5 * base * altura
    }
    
    var perímetro: Double {
        let lado1 = sqrt(base * base + altura * altura)
        let lado2 = sqrt(base * base + altura * altura)
        return base + lado1 + lado2
    }
}

// Función para calcular el área total de una lista de figuras geométricas
func calcularÁreaTotal(figuras: [FiguraGeométrica]) -> Double {
    var áreaTotal = 0.0
    
    for figura in figuras {
        áreaTotal += figura.área
    }
    
    return áreaTotal
}

// Función para calcular el perímetro total de una lista de figuras geométricas
func calcularPerímetroTotal(figuras: [FiguraGeométrica]) -> Double {
    var perímetroTotal = 0.0
    
    for figura in figuras {
        perímetroTotal += figura.perímetro
    }
    
    return perímetroTotal
}

// Creación de una lista de figuras geométricas
var figuras: [FiguraGeométrica] = []

// Adición de un círculo a la lista
figuras.append(Círculo(radio: 2.0))

// Adición de un rectángulo a la lista
figuras.append(Rectángulo(ancho: 3.0, largo: 4.0))

// Adición de un triángulo a la lista
figuras.append(Triángulo(base: 5.0, altura: 6.0))

// Cálculo del área total de la lista de figuras geométricas
let áreaTotal = calcularÁreaTotal(figuras: figuras)

// Cálculo del perímetro total de la lista de figuras geométricas
let perímetroTotal = calcularPerímetroTotal(figuras: figuras)

// Impresión de los resultados
print("El área total de las figuras geométricas es: \(áreaTotal)")
print("El perímetro total de las figuras geométricas es: \(perímetroTotal)")
```

Explicación del código:

* La primera parte del código define un protocolo llamado FiguraGeométrica. Este protocolo define tres propiedades: `nombre`, `área` y `perímetro`. Estas propiedades son necesarias para representar cualquier figura geométrica.
* A continuación, se definen tres clases que implementan el protocolo FiguraGeométrica: Círculo, Rectángulo y Triángulo. Cada clase tiene sus propias propiedades y métodos para calcular el área y el perímetro de la figura.
* La siguiente parte del código define dos funciones: calcularÁreaTotal y calcularPerímetroTotal. Estas funciones toman una lista de figuras geométricas y calculan el área total y el perímetro total respectivamente.
* La última parte del código crea una lista de figuras geométricas y luego llama a las funciones calcularÁreaTotal y calcularPerímetroTotal para calcular el área total y el perímetro total de la lista. Los resultados se imprimen en la consola.