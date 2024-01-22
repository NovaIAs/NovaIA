```swift
// Importación de librerías
import Foundation
import UIKit

// Definir protocol
protocol Superficie {
    var area: Double { get }
    var perimetro: Double { get }
}

// Definiendo clases
class Cuadrado: Superficie {
    var lado: Double

    init(lado: Double) {
        self.lado = lado
    }

    var area: Double {
        return lado * lado
    }

    var perimetro: Double {
        return 4 * lado
    }
}

class Rectángulo: Superficie {
    var ancho: Double
    var alto: Double

    init(ancho: Double, alto: Double) {
        self.ancho = ancho
        self.alto = alto
    }

    var area: Double {
        return ancho * alto
    }

    var perimetro: Double {
        return 2 * (ancho + alto)
    }
}

class Círculo: Superficie {
    var radio: Double

    init(radio: Double) {
        self.radio = radio
    }

    var area: Double {
        return Double.pi * radio * radio
    }

    var perimetro: Double {
        return 2 * Double.pi * radio
    }
}

// Funciones para interactuar con las clases

// Función para mostrar las dimensiones y el área/perímetro de una figura.
func mostrarFigura(figura: Superficie) {
    print("Tipo de figura: \(type(of: figura))")

    if let cuadrado = figura as? Cuadrado {
        print("Lado: \(cuadrado.lado)")
    } else if let rectángulo = figura as? Rectángulo {
        print("Ancho: \(rectángulo.ancho)")
        print("Alto: \(rectángulo.alto)")
    } else if let círculo = figura as? Círculo {
        print("Radio: \(círculo.radio)")
    }

    print("Área: \(figura.area)")
    print("Perímetro: \(figura.perimetro)")
    print()
}

// Crear instancias de las clases
let cuadrado = Cuadrado(lado: 5.0)
let rectángulo = Rectángulo(ancho: 4.0, alto: 6.0)
let círculo = Círculo(radio: 3.0)

// Mostrar las dimensiones y el área/perímetro de cada figura
mostrarFigura(figura: cuadrado)
mostrarFigura(figura: rectángulo)
mostrarFigura(figura: círculo)
```

**Explicación del código:**

1. Creamos un protocolo llamado **Superficie** que define dos propiedades: **area** y **perimetro**. Estas propiedades calculan el área y el perímetro de una figura geométrica.
2. Creamos tres clases: **Cuadrado**, **Rectángulo** y **Círculo**. Estas clases implementan el protocolo **Superficie** y proporcionan las implementaciones de las propiedades **area** y **perimetro** para cada figura geométrica.
3. Creamos una función llamada **mostrarFigura** que toma una instancia de cualquier clase que implemente el protocolo **Superficie** y muestra las dimensiones y el área/perímetro de la figura.
4. Creamos instancias de las clases **Cuadrado**, **Rectángulo** y **Círculo** y las pasamos a la función **mostrarFigura**.
5. La función **mostrarFigura** muestra las dimensiones y el área/perímetro de cada figura.