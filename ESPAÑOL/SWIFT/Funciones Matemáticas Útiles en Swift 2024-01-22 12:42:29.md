```swift
// Función que calcula el factorial de un número.
func factorial(_ n: Int) -> Int {
    if n == 0 {
        return 1
    } else {
        return n * factorial(n - 1)
    }
}

// Función que calcula el índice de masa corporal (IMC) de una persona.
func imc(_ peso: Double, altura: Double) -> Double {
    return peso / (altura * altura)
}

// Función que calcula la distancia entre dos puntos en un plano cartesiano.
func distancia(_ x1: Double, _ y1: Double, _ x2: Double, _ y2: Double) -> Double {
    return sqrt(pow(x2 - x1, 2) + pow(y2 - y1, 2))
}

// Función que calcula el área de un círculo.
func areaCirculo(_ radio: Double) -> Double {
    return Double.pi * radio * radio
}

// Función que calcula el perímetro de un círculo.
func perimetroCirculo(_ radio: Double) -> Double {
    return 2 * Double.pi * radio
}

// Función que calcula el volumen de una esfera.
func volumenEsfera(_ radio: Double) -> Double {
    return (4 / 3) * Double.pi * radio * radio * radio
}

// Función que calcula la superficie de una esfera.
func superficieEsfera(_ radio: Double) -> Double {
    return 4 * Double.pi * radio * radio
}

// Función que calcula el área de un triángulo.
func areaTriangulo(_ base: Double, altura: Double) -> Double {
    return 0.5 * base * altura
}

// Función que calcula el perímetro de un triángulo.
func perimetroTriangulo(_ a: Double, _ b: Double, _ c: Double) -> Double {
    return a + b + c
}

// Función que calcula el área de un cuadrado.
func areaCuadrado(_ lado: Double)  -> Double{
    return lado * lado
}

// Función que calcula el perímetro de un cuadrado.
func perimetroCuadrado(_ lado: Double) -> Double {
    return 4 * lado
}

// Función que calcula el área de un rectángulo.
func areaRectangulo(_ base: Double, _ altura: Double) -> Double {
    return base * altura
}

// Función que calcula el perímetro de un rectángulo.
func perimetroRectangulo(_ base: Double, _ altura: Double) -> Double {
    return 2 * base + 2 * altura
}
```

Este código contiene una serie de funciones matemáticas útiles, que se explican a continuación:

* `factorial`: Calcula el factorial de un número.
* `imc`: Calcula el índice de masa corporal (IMC) de una persona.
* `distancia`: Calcula la distancia entre dos puntos en un plano cartesiano.
* `areaCirculo`: Calcula el área de un círculo.
* `perimetroCirculo`: Calcula el perímetro de un círculo.
* `volumenEsfera`: Calcula el volumen de una esfera.
* `superficieEsfera`: Calcula la superficie de una esfera.
* `areaTriangulo`: Calcula el área de un triángulo.
* `perimetroTriangulo`: Calcula el perímetro de un triángulo.
* `areaCuadrado`: Calcula el área de un cuadrado.
* `perimetroCuadrado`: Calcula el perímetro de un cuadrado.
* `areaRectangulo`: Calcula el área de un rectángulo.
* `perimetroRectangulo`: Calcula el perímetro de un rectángulo.

Estas funciones se pueden utilizar para realizar cálculos matemáticos complejos en Swift.