```swift
// Declaración de una estructura que representa un punto en el espacio
struct Punto {
    var x: Double
    var y: Double
    var z: Double
    
    // Función para calcular la distancia entre dos puntos
    func distancia(a otroPunto: Punto) -> Double {
        let dx = x - otroPunto.x
        let dy = y - otroPunto.y
        let dz = z - otroPunto.z
        return sqrt(dx*dx + dy*dy + dz*dz)
    }
}

// Declaración de una clase que representa un polígono
class Poligono {
    var puntos: [Punto]
    
    // Función para calcular el área del polígono
    func area() -> Double {
        var area = 0.0
        for i in 1..<puntos.count {
            let p1 = puntos[i-1]
            let p2 = puntos[i]
            area += (p1.x * p2.y - p2.x * p1.y)
        }
        let p1 = puntos[puntos.count-1]
        let p2 = puntos[0]
        area += (p1.x * p2.y - p2.x * p1.y)
        return abs(area) / 2.0
    }
    
    // Función para calcular el perímetro del polígono
    func perimetro() -> Double {
        var perimetro = 0.0
        for i in 1..<puntos.count {
            let p1 = puntos[i-1]
            let p2 = puntos[i]
            perimetro += p1.distancia(a: p2)
        }
        let p1 = puntos[puntos.count-1]
        let p2 = puntos[0]
        perimetro += p1.distancia(a: p2)
        return perimetro
    }
}

// Declaración de una función que crea un polígono regular
func crearPoligonoRegular(lados: Int, radio: Double) -> Poligono {
    var puntos: [Punto] = []
    for i in 0..<lados {
        let angulo = 2.0 * Double.pi * Double(i) / Double(lados)
        let punto = Punto(x: radio * cos(angulo), y: radio * sin(angulo), z: 0.0)
        puntos.append(punto)
    }
    return Poligono(puntos: puntos)
}

// Declaración de una función que calcula el área y el perímetro de un polígono
func calcularAreaYPerimetro(poligono: Poligono) -> (area: Double, perimetro: Double) {
    let area = poligono.area()
    let perimetro = poligono.perimetro()
    return (area: area, perimetro: perimetro)
}

// Ejemplo de uso de las funciones
let poligono = crearPoligonoRegular(lados: 5, radio: 10.0)
let resultado = calcularAreaYPerimetro(poligono: poligono)
print("Área:", resultado.area)
print("Perímetro:", resultado.perimetro)
```

Este código es un ejemplo de cómo crear un polígono regular en 2D y calcular su área y perímetro. El código utiliza estructuras y clases para representar los puntos y el polígono, y funciones para crear el polígono y calcular su área y perímetro. El código también incluye un ejemplo de cómo usar estas funciones para crear un pentágono regular y calcular su área y perímetro.

El código es complejo y diferenciado porque utiliza estructuras, clases, funciones y algoritmos para crear y calcular el área y perímetro de un polígono. El código también es difícil de repetir porque utiliza conceptos avanzados de programación en Swift, como estructuras, clases y funciones.