Aquí hay un código complejo en Swift que incluye diversas características y funcionalidades:

```swift
// Importar bibliotecas necesarias
import Foundation

// Definir una estructura para representar un punto en el espacio
struct Punto {
    var x: Double
    var y: Double
}

// Definir una clase para representar una línea recta
class Línea {
    var puntoInicial: Punto
    var puntoFinal: Punto

    // Inicializar la línea con puntos iniciales y finales
    init(puntoInicial: Punto, puntoFinal: Punto) {
        self.puntoInicial = puntoInicial
        self.puntoFinal = puntoFinal
    }

    // Calcular la longitud de la línea
    func longitud() -> Double {
        let distanciaX = puntoFinal.x - puntoInicial.x
        let distanciaY = puntoFinal.y - puntoInicial.y
        return sqrt(distanciaX * distanciaX + distanciaY * distanciaY)
    }
}

// Definir una función para calcular la pendiente de una línea
func pendiente(paraLinea linea: Línea) -> Double {
    let distanciaX = linea.puntoFinal.x - linea.puntoInicial.x
    let distanciaY = linea.puntoFinal.y - linea.puntoInicial.y
    return distanciaY / distanciaX
}

// Definir una clase para representar un círculo
class Círculo {
    var centro: Punto
    var radio: Double

    // Inicializar el círculo con un centro y un radio
    init(centro: Punto, radio: Double) {
        self.centro = centro
        self.radio = radio
    }

    // Calcular el área del círculo
    func area() -> Double {
        return Double.pi * radio * radio
    }
}

// Definir una función para calcular la distancia entre dos puntos
func distancia(entre punto1: Punto, y punto2: Punto) -> Double {
    let distanciaX = punto2.x - punto1.x
    let distanciaY = punto2.y - punto1.y
    return sqrt(distanciaX * distanciaX + distanciaY * distanciaY)
}

// Crear algunos objetos para probar las funciones y clases definidas
let punto1 = Punto(x: 0, y: 0)
let punto2 = Punto(x: 3, y: 4)

let línea1 = Línea(puntoInicial: punto1, puntoFinal: punto2)

let círculo1 = Círculo(centro: punto1, radio: 5)

// Imprimir los resultados de las funciones y clases
print("Longitud de la línea: \(línea1.longitud())")
print("Pendiente de la línea: \(pendiente(paraLinea: línea1))")
print("Área del círculo: \(círculo1.area())")
print("Distancia entre dos puntos: \(distancia(entre: punto1, y: punto2))")
```

Explicación del código:

1. Importamos las bibliotecas necesarias, como `Foundation`.

2. Definimos una estructura `Punto` para representar un punto en el espacio.

3. Definimos una clase `Línea` para representar una línea recta entre dos puntos.

4. Definimos una función `pendiente` para calcular la pendiente de una línea dada.

5. Definimos una clase `Círculo` para representar un círculo con un centro y un radio.

6. Definimos una función `area` para calcular el área de un círculo dado.

7. Definimos una función `distancia` para calcular la distancia entre dos puntos dados.

8. Creamos algunos objetos para probar las funciones y clases definidas, como puntos, líneas y círculos.

9. Imprimimos los resultados de las funciones y clases para mostrar su funcionamiento.

Este código cubre una variedad de conceptos y técnicas en Swift, incluyendo estructuras, clases, funciones, inicializadores, propiedades, métodos, cálculos matemáticos, y más. También muestra cómo crear y utilizar objetos en Swift para representar y manipular datos.