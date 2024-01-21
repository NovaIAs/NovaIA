```swift
// Protocolo para definir una interfaz común para diferentes tipos de figuras geométricas
protocol FiguraGeométrica {
    var área: Double { get }
    var perímetro: Double { get }
    func dibujar()
}

// Clase que implementa el protocolo FiguraGeométrica para un círculo
class Círculo: FiguraGeométrica {
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

    func dibujar() {
        print("Dibujando un círculo con radio \(radio)")
    }
}

// Clase que implementa el protocolo FiguraGeométrica para un rectángulo
class Rectángulo: FiguraGeométrica {
    var base: Double
    var altura: Double

    init(base: Double, altura: Double) {
        self.base = base
        self.altura = altura
    }

    var área: Double {
        return base * altura
    }

    var perímetro: Double {
        return 2 * (base + altura)
    }

    func dibujar() {
        print("Dibujando un rectángulo con base \(base) y altura \(altura)")
    }
}

// Clase que implementa el protocolo FiguraGeométrica para un triángulo
class Triángulo: FiguraGeométrica {
    var base: Double
    var altura: Double
    var lado1: Double
    var lado2: Double

    init(base: Double, altura: Double, lado1: Double, lado2: Double) {
        self.base = base
        self.altura = altura
        self.lado1 = lado1
        self.lado2 = lado2
    }

    var área: Double {
        return 0.5 * base * altura
    }

    var perímetro: Double {
        return base + lado1 + lado2
    }

    func dibujar() {
        print("Dibujando un triángulo con base \(base), altura \(altura), lado1 \(lado1) y lado2 \(lado2)")
    }
}

// Función que recibe una lista de figuras geométricas y calcula el área total de todas ellas
func áreaTotal(figuras: [FiguraGeométrica]) -> Double {
    var áreaTotal = 0.0
    for figura in figuras {
        áreaTotal += figura.área
    }
    return áreaTotal
}

// Función que recibe una lista de figuras geométricas y calcula el perímetro total de todas ellas
func perímetroTotal(figuras: [FiguraGeométrica]) -> Double {
    var perímetroTotal = 0.0
    for figura in figuras {
        perímetroTotal += figura.perímetro
    }
    return perímetroTotal
}

// Ejemplo de uso de las clases y funciones anteriores
let círculo1 = Círculo(radio: 5.0)
let rectángulo1 = Rectángulo(base: 10.0, altura: 5.0)
let triángulo1 = Triángulo(base: 10.0, altura: 8.0, lado1: 12.0, lado2: 15.0)

let figuras: [FiguraGeométrica] = [círculo1, rectángulo1, triángulo1]

print("Área total de las figuras: \(áreaTotal(figuras: figuras))")
print("Perímetro total de las figuras: \(perímetroTotal(figuras: figuras))")

for figura in figuras {
    figura.dibujar()
}
```

**Explicación del código:**

* El código anterior define un protocolo llamado `FiguraGeométrica` que define una interfaz común para diferentes tipos de figuras geométricas. Este protocolo incluye dos propiedades, `área` y `perímetro`, y un método, `dibujar`.
* Las clases `Círculo`, `Rectángulo` y `Triángulo` implementan el protocolo `FiguraGeométrica` para proporcionar implementaciones concretas de las propiedades y el método definidos en el protocolo.
* Las funciones `áreaTotal` y `perímetroTotal` reciben una lista de figuras geométricas y calculan el área total y el perímetro total de todas ellas, respectivamente.
* El último bloque de código crea instancias de las clases `Círculo`, `Rectángulo` y `Triángulo`, las añade a una lista de figuras geométricas y llama a las funciones `áreaTotal` y `perímetroTotal` para calcular el área total y el perímetro total de todas ellas.
* Finalmente, el código llama al método `dibujar` de cada figura geométrica para dibujarlas en la consola.