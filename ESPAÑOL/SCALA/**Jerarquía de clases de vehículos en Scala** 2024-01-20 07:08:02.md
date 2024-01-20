```scala
// Clase de vehículo que define propiedades comunes a todos los vehículos
class Vehiculo {
  var marca: String = ""
  var modelo: String = ""
  var año: Int = 0

  def arrancar(): Unit = {
    println("El vehículo está arrancando...")
  }

  def conducir(): Unit = {
    println("El vehículo está conduciendo...")
  }

  def detenerse(): Unit = {
    println("El vehículo se está deteniendo...")
  }
}

// Clase de coche que hereda de la clase Vehiculo y añade propiedades específicas de los coches
class Coche extends Vehiculo {
  var numPuertas: Int = 0
  var tipoCombustible: String = ""

  override def arrancar(): Unit = {
    super.arrancar()
    println("El coche está acelerando...")
  }

  override def conducir(): Unit = {
    super.conducir()
    println("El coche está girando...")
  }

  override def detenerse(): Unit = {
    super.detenerse()
    println("El coche está frenando...")
  }
}

// Clase de camión que hereda de la clase Vehiculo y añade propiedades específicas de los camiones
class Camión extends Vehiculo {
  var capacidadCarga: Int = 0
  var tipoCarga: String = ""

  override def arrancar(): Unit = {
    super.arrancar()
    println("El camión está arrancando...")
  }

  override def conducir(): Unit = {
    super.conducir()
    println("El camión está transportando carga...")
  }

  override def detenerse(): Unit = {
    super.detenerse()
    println("El camión está descargando carga...")
  }
}

// Clase de moto que hereda de la clase Vehiculo y añade propiedades específicas de las motos
class Moto extends Vehiculo {
  var cilindrada: Int = 0
  var tipoMoto: String = ""

  override def arrancar(): Unit = {
    super.arrancar()
    println("La moto está acelerando...")
  }

  override def conducir(): Unit = {
    super.conducir()
    println("La moto está cambiando de marcha...")
  }

  override def detenerse(): Unit = {
    super.detenerse()
    println("La moto está frenando...")
  }
}

// Función main que crea objetos de las diferentes clases y llama a sus métodos
object Main {
  def main(args: Array[String]): Unit = {
    val coche = new Coche()
    coche.marca = "Toyota"
    coche.modelo = "Yaris"
    coche.año = 2023
    coche.numPuertas = 4
    coche.tipoCombustible = "Gasolina"

    coche.arrancar()
    coche.conducir()
    coche.detenerse()

    println()

    val camión = new Camión()
    camión.marca = "Volvo"
    camión.modelo = "FH16"
    camión.año = 2021
    camión.capacidadCarga = 10000
    camión.tipoCarga = "Materiales de construcción"

    camión.arrancar()
    camión.conducir()
    camión.detenerse()

    println()

    val moto = new Moto()
    moto.marca = "Honda"
    moto.modelo = "CBR1000RR"
    moto.año = 2022
    moto.cilindrada = 1000
    moto.tipoMoto = "Deportiva"

    moto.arrancar()
    moto.conducir()
    moto.detenerse()
  }
}
```

Explicación del código:

- **Clases**: Se definen tres clases, `Vehiculo`, `Coche`, `Camión` y `Moto`, que representan a los diferentes tipos de vehículos. La clase `Vehiculo` define las propiedades y métodos comunes a todos los vehículos, mientras que las clases `Coche`, `Camión` y `Moto` heredan de la clase `Vehiculo` y añaden propiedades y métodos específicos de cada tipo de vehículo.

- **Propiedades**: Cada clase tiene sus propias propiedades, que representan las características específicas de cada tipo de vehículo. Por ejemplo, la clase `Coche` tiene propiedades como `numPuertas` y `tipoCombustible`, mientras que la clase `Camión` tiene propiedades como `capacidadCarga` y `tipoCarga`.

- **Métodos**: Cada clase tiene sus propios métodos, que representan las acciones que puede realizar cada tipo de vehículo. Por ejemplo, la clase `Vehiculo` tiene métodos como `arrancar()`, `conducir()` y `detenerse()`, mientras que la clase `Coche` tiene métodos como `acelerar()` y `girar()`.

- **Función main**: La función `main()` crea objetos de las diferentes clases y llama a sus métodos para demostrar cómo funcionan. Por ejemplo, crea un objeto de la clase `Coche`, establece sus propiedades, y llama a sus métodos para arrancar, conducir y detenerse.

Este código muestra cómo utilizar la herencia y el polimorfismo en Scala para crear una jerarquía de clases que representan diferentes tipos de vehículos. También muestra cómo utilizar los objetos para almacenar datos y llamar a métodos para realizar acciones.