```swift
// Definimos una estructura para representar un punto en el espacio tridimensional.
struct Punto3D {
    var x: Double
    var y: Double
    var z: Double

    // Inicializamos el punto con coordenadas (0, 0, 0).
    init() {
        self.x = 0.0
        self.y = 0.0
        self.z = 0.0
    }

    // Inicializamos el punto con coordenadas específicas.
    init(x: Double, y: Double, z: Double) {
        self.x = x
        self.y = y
        self.z = z
    }

    // Calculamos la distancia euclidiana entre dos puntos.
    func distancia(a otroPunto: Punto3D) -> Double {
        let dx = self.x - otroPunto.x
        let dy = self.y - otroPunto.y
        let dz = self.z - otroPunto.z

        return sqrt(dx * dx + dy * dy + dz * dz)
    }
}

// Definimos una clase para representar una línea en el espacio tridimensional.
class Linea3D {
    var puntoInicial: Punto3D
    var puntoFinal: Punto3D

    // Inicializamos la línea con dos puntos.
    init(puntoInicial: Punto3D, puntoFinal: Punto3D) {
        self.puntoInicial = puntoInicial
        self.puntoFinal = puntoFinal
    }

    // Calculamos la longitud de la línea.
    func longitud() -> Double {
        return self.puntoInicial.distancia(a: self.puntoFinal)
    }

    // Calculamos el punto medio de la línea.
    func puntoMedio() -> Punto3D {
        let xMedio = (self.puntoInicial.x + self.puntoFinal.x) / 2.0
        let yMedio = (self.puntoInicial.y + self.puntoFinal.y) / 2.0
        let zMedio = (self.puntoInicial.z + self.puntoFinal.z) / 2.0

        return Punto3D(x: xMedio, y: yMedio, z: zMedio)
    }
}

// Definimos una clase para representar un plano en el espacio tridimensional.
class Plano3D {
    var punto1: Punto3D
    var punto2: Punto3D
    var punto3: Punto3D

    // Inicializamos el plano con tres puntos.
    init(punto1: Punto3D, punto2: Punto3D, punto3: Punto3D) {
        self.punto1 = punto1
        self.punto2 = punto2
        self.punto3 = punto3
    }

    // Calculamos el área del plano.
    func area() -> Double {
        let v1 = Punto3D(x: self.punto2.x - self.punto1.x, y: self.punto2.y - self.punto1.y, z: self.punto2.z - self.punto1.z)
        let v2 = Punto3D(x: self.punto3.x - self.punto1.x, y: self.punto3.y - self.punto1.y, z: self.punto3.z - self.punto1.z)

        let productoVectorial = Punto3D(x: v1.y * v2.z - v1.z * v2.y, y: v1.z * v2.x - v1.x * v2.z, z: v1.x * v2.y - v1.y * v2.x)

        return 0.5 * sqrt(productoVectorial.x * productoVectorial.x + productoVectorial.y * productoVectorial.y + productoVectorial.z * productoVectorial.z)
    }

    // Calculamos la distancia entre un punto y el plano.
    func distancia(a punto: Punto3D) -> Double {
        let n = Punto3D(x: self.punto2.x - self.punto1.x, y: self.punto2.y - self.punto1.y, z: self.punto2.z - self.punto1.z)
        let d = -(n.x * self.punto1.x + n.y * self.punto1.y + n.z * self.punto1.z)

        return abs(n.x * punto.x + n.y * punto.y + n.z * punto.z + d) / sqrt(n.x * n.x + n.y * n.y + n.z * n.z)
    }
}

// Definimos una clase para representar una esfera en el espacio tridimensional.
class Esfera3D {
    var centro: Punto3D
    var radio: Double

    // Inicializamos la esfera con un centro y un radio.
    init(centro: Punto3D, radio: Double) {
        self.centro = centro
        self.radio = radio
    }

    // Calculamos el volumen de la esfera.
    func volumen() -> Double {
        return (4.0 / 3.0) * Double.pi * self.radio * self.radio * self.radio
    }

    // Calculamos la superficie de la esfera.
    func superficie() -> Double {
        return 4.0 * Double.pi * self.radio * self.radio
    }

    // Calculamos la distancia entre un punto y la esfera.
    func distancia(a punto: Punto3D) -> Double {
        return sqrt((punto.x - self.centro.x) * (punto.x - self.centro.x) + (punto.y - self.centro.y) * (punto.y - self.centro.y) + (punto.z - self.centro.z) * (punto.z - self.centro.z)) - self.radio
    }
}

// Creamos un punto en el espacio tridimensional.
let punto1 = Punto3D(x: 1.0, y: 2.0, z: 3.0)

// Creamos una línea en el espacio tridimensional.
let linea1 = Linea3D(puntoInicial: Punto3D(x: 0.0, y: 0.0, z: 0.0), puntoFinal: Punto3D(x: 10.0, y: 10.0, z: 10.0))

// Creamos un plano en el espacio tridimensional.
let plano1 = Plano3D(punto1: Punto3D(x: 0.0, y: 0.0, z: 0.0), punto2: Punto3D(x: 10.0, y: 0.0, z: 0.0), punto3: Punto3D(x: 0.0, y: 10.0, z: 0.0))

// Creamos una esfera en el espacio tridimensional.
let esfera1 = Esfera3D(centro: Punto3D(x: 0.0, y