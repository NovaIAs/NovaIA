```swift
// Definir una clase para representar un vehículo.
class Vehiculo {
    var marca: String
    var modelo: String
    var año: Int

    // Inicializador para crear un nuevo vehículo.
    init(marca: String, modelo: String, año: Int) {
        self.marca = marca
        self.modelo = modelo
        self.año = año
    }

    // Función para imprimir la información del vehículo.
    func imprimir() {
        print("Marca: \(marca)")
        print("Modelo: \(modelo)")
        print("Año: \(año)")
    }
}

// Definir una clase para representar un automóvil.
class Automovil: Vehiculo {
    var numeroPuertas: Int

    // Inicializador para crear un nuevo automóvil.
    init(marca: String, modelo: String, año: Int, numeroPuertas: Int) {
        super.init(marca: marca, modelo: modelo, año: año)
        self.numeroPuertas = numeroPuertas
    }

    // Reemplazar la función imprimir para incluir el número de puertas.
    override func imprimir() {
        super.imprimir()
        print("Número de puertas: \(numeroPuertas)")
    }
}

// Definir una clase para representar una camioneta.
class Camioneta: Vehiculo {
    var capacidadCarga: Int

    // Inicializador para crear una nueva camioneta.
    init(marca: String, modelo: String, año: Int, capacidadCarga: Int) {
        super.init(marca: marca, modelo: modelo, año: año)
        self.capacidadCarga = capacidadCarga
    }

    // Reemplazar la función imprimir para incluir la capacidad de carga.
    override func imprimir() {
        super.imprimir()
        print("Capacidad de carga: \(capacidadCarga) kg")
    }
}

// Definir una clase para representar una motocicleta.
class Motocicleta: Vehiculo {
    var cilindrada: Int

    // Inicializador para crear una nueva motocicleta.
    init(marca: String, modelo: String, año: Int, cilindrada: Int) {
        super.init(marca: marca, modelo: modelo, año: año)
        self.cilindrada = cilindrada
    }

    // Reemplazar la función imprimir para incluir la cilindrada.
    override func imprimir() {
        super.imprimir()
        print("Cilindrada: \(cilindrada) cc")
    }
}

// Crear una lista de vehículos.
var vehiculos: [Vehiculo] = []

// Agregar algunos vehículos a la lista.
vehiculos.append(Automovil(marca: "Honda", modelo: "Civic", año: 2020, numeroPuertas: 4))
vehiculos.append(Camioneta(marca: "Ford", modelo: "F-150", año: 2021, capacidadCarga: 1000))
vehiculos.append(Motocicleta(marca: "Harley-Davidson", modelo: "Sportster", año: 2022, cilindrada: 1200))

// Imprimir la información de cada vehículo en la lista.
for vehiculo in vehiculos {
    vehiculo.imprimir()
    print()
}
```

**Explicación:**

Este código define varias clases para representar diferentes tipos de vehículos, como automóviles, camionetas y motocicletas. Cada clase tiene sus propios atributos y métodos, que se utilizan para representar las características específicas de cada tipo de vehículo.

También se define una lista de vehículos y se agregan algunos vehículos a la lista. Luego, se itera sobre la lista y se imprime la información de cada vehículo.

Este código es un ejemplo de polimorfismo, que es una característica fundamental de la programación orientada a objetos. El polimorfismo permite que objetos de diferentes clases compartan la misma interfaz, de modo que puedan ser tratados de manera uniforme por el programa.