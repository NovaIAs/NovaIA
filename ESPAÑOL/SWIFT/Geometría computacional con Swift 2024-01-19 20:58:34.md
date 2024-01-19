```swift
// Definición de la clase Point
class Punto {
    var x: Double
    var y: Double

    init(x: Double, y: Double) {
        self.x = x
        self.y = y
    }

    func distancia(a otroPunto: Punto) -> Double {
        let dx = self.x - otroPunto.x
        let dy = self.y - otroPunto.y
        return sqrt(dx*dx + dy*dy)
    }
}

// Definición de la clase Rectángulo
class Rectángulo {
    var origin: Punto
    var size: Punto

    init(origin: Punto, size: Punto) {
        self.origin = origin
        self.size = size
    }

    func area() -> Double {
        return self.size.x * self.size.y
    }

    func perímetro() -> Double {
        return 2 * (self.size.x + self.size.y)
    }
}

// Definición de la clase Círculo
class Círculo {
    var center: Punto
    var radius: Double

    init(center: Punto, radius: Double) {
        self.center = center
        self.radius = radius
    }

    func area() -> Double {
        return Double.pi * self.radius * self.radius
    }

    func perímetro() -> Double {
        return 2 * Double.pi * self.radius
    }
}

// Definición de la clase Figura
class Figura {
    var tipo: String
    var puntos: [Punto]

    init(tipo: String, puntos: [Punto]) {
        self.tipo = tipo
        self.puntos = puntos
    }

    func area() -> Double {
        return 0.0 // Área no calculada para figuras arbitrarias
    }

    func perímetro() -> Double {
        return 0.0 // Perímetro no calculado para figuras arbitrarias
    }
}

// Creación de objetos
let punto1 = Punto(x: 0, y: 0)
let punto2 = Punto(x: 3, y: 4)
let rectángulo = Rectángulo(origin: punto1, size: punto2)
let círculo = Círculo(center: punto1, radius: 5)
let figuras: [Figura] = [rectángulo, círculo]

// Cálculo del área y perímetro de las figuras
for figura in figuras {
    print("Tipo: \(figura.tipo)")
    print("Área: \(figura.area())")
    print("Perímetro: \(figura.perímetro())")
    print()
}
```

Este código crea tres clases: `Punto`, `Rectángulo` y `Círculo`, que representan puntos, rectángulos y círculos en un plano bidimensional. También crea una clase `Figura` que representa una figura arbitraria.

Luego crea objetos de estas clases y los agrega a una lista de figuras. Finalmente, calcula el área y perímetro de cada figura y los imprime en la consola.