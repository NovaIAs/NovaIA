```scala
// Definición de una clase Persona con propiedades y métodos
class Persona(nombre: String, apellido: String, edad: Int) {
  // Propiedades
  val nombre: String = nombre
  val apellido: String = apellido
  var edad: Int = edad

  // Métodos
  def saludar(): String = {
    s"Hola, soy $nombre $apellido y tengo $edad años."
  }

  def cumplirAnios(): Unit = {
    edad += 1
  }
}

// Definición de una clase Empleado que hereda de Persona y añade propiedades y métodos específicos
class Empleado(nombre: String, apellido: String, edad: Int, cargo: String, salario: Double) extends Persona(nombre, apellido, edad) {
  // Propiedades
  val cargo: String = cargo
  var salario: Double = salario

  // Métodos
  def trabajar(): String = {
    s"Estoy trabajando como $cargo y gano $salario euros al mes."
  }

  def pedirAumento(porcentaje: Double): Unit = {
    salario += salario * porcentaje / 100
  }
}

// Definición de una clase Empresa que tiene una lista de empleados
class Empresa(nombre: String, empleados: List[Empleado]) {
  // Propiedades
  val nombre: String = nombre
  var empleados: List[Empleado] = empleados

  // Métodos
  def contratarEmpleado(empleado: Empleado): Unit = {
    empleados = empleado :: empleados
  }

  def despedirEmpleado(empleado: Empleado): Unit = {
    empleados = empleados.filterNot(_ == empleado)
  }

  def calcularNomina(): Double = {
    empleados.map(_.salario).sum
  }
}

// Crear una empresa y añadir empleados
val empresa = new Empresa("Acme Corporation", List())

val empleado1 = new Empleado("Juan", "García", 25, "Ingeniero de software", 2000)
val empleado2 = new Empleado("María", "López", 30, "Contable", 2500)
val empleado3 = new Empleado("Pedro", "Sánchez", 40, "Director general", 5000)

empresa.contratarEmpleado(empleado1)
empresa.contratarEmpleado(empleado2)
empresa.contratarEmpleado(empleado3)

// Despedir a un empleado
empresa.despedirEmpleado(empleado2)

// Calcular la nómina de la empresa
val nomina = empresa.calcularNomina()

// Mostrar la información de la empresa y sus empleados
println(s"Nombre de la empresa: ${empresa.nombre}")
println(s"Número de empleados: ${empresa.empleados.size}")
println(s"Nomina total: ${nomina} euros")

for (empleado <- empresa.empleados) {
  println(s"Nombre: ${empleado.nombre} ${empleado.apellido}")
  println(s"Edad: ${empleado.edad}")
  println(s"Cargo: ${empleado.cargo}")
  println(s"Salario: ${empleado.salario} euros")
  println()
}
```

**Explicación del código:**

* Definimos tres clases: `Persona`, `Empleado` y `Empresa`.
* En la clase `Persona` definimos dos propiedades, `nombre` y `apellido`, que son inmutables, y una propiedad mutable, `edad`. También definimos dos métodos, `saludar()` y `cumplirAnios()`.
* En la clase `Empleado` heredamos de la clase `Persona` y añadimos dos propiedades, `cargo` y `salario`, y un método, `trabajar()`.
* En la clase `Empresa` definimos una propiedad, `nombre`, y una propiedad mutable, `empleados`. También definimos tres métodos, `contratarEmpleado()`, `despedirEmpleado()` y `calcularNomina()`.
* Creamos una empresa y añadimos tres empleados.
* Despedimos a uno de los empleados.
* Calculamos la nómina de la empresa.
* Mostramos la información de la empresa y sus empleados.

Este código es complejo porque consta de varias clases, con sus respectivas propiedades y métodos, y utiliza herencia y polimorfismo. También utiliza listas y expresiones lambda para procesar datos.