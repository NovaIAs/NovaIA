```scala
// Definición de la clase Persona con sus atributos y métodos
class Persona(val nombre: String, var edad: Int) {
  // Método para obtener el nombre de la persona
  def getNombre(): String = {
    nombre
  }

  // Método para obtener la edad de la persona
  def getEdad(): Int = {
    edad
  }

  // Método para incrementar la edad de la persona en un año
  def envejecer(): Unit = {
    edad += 1
  }
}

// Definición de la clase Familia con sus atributos y métodos
class Familia(val miembros: List[Persona]) {
  // Método para obtener el número de miembros de la familia
  def getNumeroMiembros(): Int = {
    miembros.size
  }

  // Método para obtener la edad promedio de los miembros de la familia
  def getEdadPromedio(): Double = {
    val edades = miembros.map(_.getEdad())
    edades.sum / edades.size
  }

  // Método para obtener el miembro más joven de la familia
  def getMiembroMasJoven(): Persona = {
    miembros.minBy(_.getEdad())
  }

  // Método para obtener el miembro más viejo de la familia
  def getMiembroMasViejo(): Persona = {
    miembros.maxBy(_.getEdad())
  }
}

// Definición de la clase Ciudad con sus atributos y métodos
class Ciudad(val nombre: String, val habitantes: List[Familia]) {
  // Método para obtener el número de habitantes de la ciudad
  def getNumeroHabitantes(): Int = {
    habitantes.size
  }

  // Método para obtener la edad promedio de los habitantes de la ciudad
  def getEdadPromedio(): Double = {
    val edades = habitantes.flatMap(_.getMiembros()).map(_.getEdad())
    edades.sum / edades.size
  }

  // Método para obtener la familia más grande de la ciudad
  def getFamiliaMasGrande(): Familia = {
    habitantes.maxBy(_.getNumeroMiembros())
  }

  // Método para obtener la familia más pequeña de la ciudad
  def getFamiliaMasPequena(): Familia = {
    habitantes.minBy(_.getNumeroMiembros())
  }
}

// Definición de la clase País con sus atributos y métodos
class Pais(val nombre: String, val ciudades: List[Ciudad]) {
  // Método para obtener el número de habitantes del país
  def getNumeroHabitantes(): Int = {
    ciudades.map(_.getNumeroHabitantes()).sum
  }

  // Método para obtener la edad promedio de los habitantes del país
  def getEdadPromedio(): Double = {
    val edades = ciudades.flatMap(_.getHabitantes()).flatMap(_.getMiembros()).map(_.getEdad())
    edades.sum / edades.size
  }

  // Método para obtener la ciudad más grande del país
  def getCiudadMasGrande(): Ciudad = {
    ciudades.maxBy(_.getNumeroHabitantes())
  }

  // Método para obtener la ciudad más pequeña del país
  def getCiudadMasPequena(): Ciudad = {
    ciudades.minBy(_.getNumeroHabitantes())
  }
}

// Creación de una instancia de la clase Persona
val persona1 = new Persona("Juan", 25)

// Creación de una instancia de la clase Familia
val familia1 = new Familia(List(persona1))

// Creación de una instancia de la clase Ciudad
val ciudad1 = new Ciudad("Madrid", List(familia1))

// Creación de una instancia de la clase País
val pais1 = new Pais("España", List(ciudad1))

// Impresión de los datos de la persona
println("Nombre: " + persona1.getNombre())
println("Edad: " + persona1.getEdad())

// Impresión de los datos de la familia
println("Número de miembros: " + familia1.getNumeroMiembros())
println("Edad promedio: " + familia1.getEdadPromedio())
println("Miembro más joven: " + familia1.getMiembroMasJoven().getNombre())
println("Miembro más viejo: " + familia1.getMiembroMasViejo().getNombre())

// Impresión de los datos de la ciudad
println("Nombre: " + ciudad1.getNombre())
println("Número de habitantes: " + ciudad1.getNumeroHabitantes())
println("Edad promedio: " + ciudad1.getEdadPromedio())
println("Familia más grande: " + ciudad1.getFamiliaMasGrande().getNumeroMiembros())
println("Familia más pequeña: " + ciudad1.getFamiliaMasPequena().getNumeroMiembros())

// Impresión de los datos del país
println("Nombre: " + pais1.getNombre())
println("Número de habitantes: " + pais1.getNumeroHabitantes())
println("Edad promedio: " + pais1.getEdadPromedio())
println("Ciudad más grande: " + pais1.getCiudadMasGrande().getNombre())
println("Ciudad más pequeña: " + pais1.getCiudadMasPequena().getNombre())
```

**Explicación del código:**

* La clase `Persona` define los atributos `nombre` y `edad`, y los métodos `getNombre`, `getEdad` y `envejecer`.
* La clase `Familia` define el atributo `miembros` y los métodos `getNumeroMiembros`, `getEdadPromedio`, `getMiembroMasJoven` y `getMiembroMasViejo`.
* La clase `Ciudad` define los atributos `nombre` y `habitantes`, y los métodos `getNumeroHabitantes`, `getEdadPromedio`, `getFamiliaMasGrande` y `getFamiliaMasPequena`.
* La clase `Pais` define los atributos `nombre` y `ciudades`, y los métodos `getNumeroHabitantes`, `getEdadPromedio`, `getCiudadMasGrande` y `getCiudadMasPequena`.
* El código crea una instancia de la clase `Persona`, una instancia de la clase `Familia`, una instancia de la clase `Ciudad` y una instancia de la clase `Pais`.
* El código imprime los datos de la persona, de la familia, de la ciudad y del país.