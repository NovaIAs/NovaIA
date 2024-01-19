```scala
// Definición de la clase Persona
class Persona(nombre: String, edad: Int, ocupacion: String) {
  def obtenerNombre: String = nombre
  def obtenerEdad: Int = edad
  def obtenerOcupacion: String = ocupacion
}

// Definición de la clase Empleado que hereda de Persona
class Empleado(nombre: String, edad: Int, ocupacion: String, sueldo: Double) extends Persona(nombre, edad, ocupacion) {
  def obtenerSueldo: Double = sueldo
}

// Definición de la clase Gerente que hereda de Empleado
class Gerente(nombre: String, edad: Int, ocupacion: String, sueldo: Double, departamento: String) extends Empleado(nombre, edad, ocupacion, sueldo) {
  def obtenerDepartamento: String = departamento
}

// Definición de la clase Empresa
class Empresa(nombre: String, direccion: String, empleados: List[Empleado]) {
  def obtenerNombre: String = nombre
  def obtenerDireccion: String = direccion
  def obtenerEmpleados: List[Empleado] = empleados
}

// Definición de la clase Main
object Main {
  def main(args: Array[String]): Unit = {
    // Crear una instancia de la clase Persona
    val persona = new Persona("Juan", 25, "Estudiante")

    // Crear una instancia de la clase Empleado
    val empleado = new Empleado("María", 30, "Ingeniera", 4000.0)

    // Crear una instancia de la clase Gerente
    val gerente = new Gerente("Pedro", 40, "Gerente General", 6000.0, "Finanzas")

    // Crear una instancia de la clase Empresa
    val empresa = new Empresa("Acme Corporation", "123 Main Street", List(empleado, gerente))

    // Imprimir el nombre de la persona
    println(persona.obtenerNombre)

    // Imprimir el sueldo del empleado
    println(empleado.obtenerSueldo)

    // Imprimir el departamento del gerente
    println(gerente.obtenerDepartamento)

    // Imprimir el nombre de la empresa
    println(empresa.obtenerNombre)

    // Imprimir la dirección de la empresa
    println(empresa.obtenerDireccion)

    // Imprimir la lista de empleados de la empresa
    empresa.obtenerEmpleados.foreach(println)
  }
}
```

**Explicación del código:**

* La clase `Persona` define las propiedades y métodos comunes a todas las personas, como el nombre, la edad y la ocupación.
* La clase `Empleado` hereda de la clase `Persona` y añade propiedades y métodos específicos de los empleados, como el sueldo.
* La clase `Gerente` hereda de la clase `Empleado` y añade propiedades y métodos específicos de los gerentes, como el departamento.
* La clase `Empresa` define las propiedades y métodos comunes a todas las empresas, como el nombre, la dirección y la lista de empleados.
* La clase `Main` es la clase principal del programa y contiene el método `main` que se ejecuta cuando se ejecuta el programa.
* En el método `main` se crean instancias de las clases `Persona`, `Empleado`, `Gerente` y `Empresa` y se imprimen sus propiedades.

Este código es un ejemplo de cómo utilizar la herencia y la composición en Scala para crear clases complejas y reutilizables.