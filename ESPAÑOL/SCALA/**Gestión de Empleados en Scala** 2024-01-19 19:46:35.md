**Problema:**

Se tiene un conjunto de datos que contiene información sobre los empleados de una empresa, incluyendo su nombre, departamento, salario y antigüedad. Se quiere crear un programa en Scala que permita al usuario realizar las siguientes operaciones:

* Mostrar todos los empleados
* Mostrar todos los empleados de un departamento específico
* Mostrar la información de un empleado específico
* Agregar un nuevo empleado
* Editar la información de un empleado
* Eliminar un empleado

**Solución:**

```scala
// Importar las librerías necesarias
import scala.collection.mutable.ListBuffer

// Crear una clase Empleado
class Empleado(val nombre: String, val departamento: String, val salario: Double, val antiguedad: Int) {

  // Método para mostrar la información del empleado
  def mostrar(): Unit = {
    println(s"Nombre: $nombre")
    println(s"Departamento: $departamento")
    println(s"Salario: $salario")
    println(s"Antigüedad: $antiguedad años")
  }
}

// Crear una clase Empresa
class Empresa {

  // Crear una lista de empleados
  private val empleados = new ListBuffer[Empleado]

  // Método para agregar un nuevo empleado
  def agregarEmpleado(empleado: Empleado): Unit = {
    empleados += empleado
  }

  // Método para editar la información de un empleado
  def editarEmpleado(empleado: Empleado): Unit = {
    val index = empleados.indexOf(empleado)
    if (index != -1) {
      empleados(index) = empleado
    }
  }

  // Método para eliminar un empleado
  def eliminarEmpleado(empleado: Empleado): Unit = {
    val index = empleados.indexOf(empleado)
    if (index != -1) {
      empleados.remove(index)
    }
  }

  // Método para mostrar todos los empleados
  def mostrarEmpleados(): Unit = {
    empleados.foreach(_.mostrar())
  }

  // Método para mostrar todos los empleados de un departamento específico
  def mostrarEmpleadosPorDepartamento(departamento: String): Unit = {
    empleados.filter(_.departamento == departamento).foreach(_.mostrar())
  }

  // Método para mostrar la información de un empleado específico
  def mostrarEmpleado(nombre: String): Unit = {
    empleados.find(_.nombre == nombre).foreach(_.mostrar())
  }
}

// Crear una instancia de la clase Empresa
val empresa = new Empresa

// Agregar algunos empleados a la empresa
empresa.agregarEmpleado(new Empleado("Juan Pérez", "Ventas", 1500.0, 5))
empresa.agregarEmpleado(new Empleado("María Gómez", "Marketing", 2000.0, 3))
empresa.agregarEmpleado(new Empleado("Pedro López", "Desarrollo", 2500.0, 1))

// Mostrar todos los empleados
empresa.mostrarEmpleados()

// Mostrar todos los empleados del departamento de Ventas
empresa.mostrarEmpleadosPorDepartamento("Ventas")

// Mostrar la información del empleado Juan Pérez
empresa.mostrarEmpleado("Juan Pérez")

// Editar la información del empleado Juan Pérez
empresa.editarEmpleado(new Empleado("Juan Pérez", "Ventas", 1800.0, 5))

// Eliminar el empleado Pedro López
empresa.eliminarEmpleado(new Empleado("Pedro López", "Desarrollo", 2500.0, 1))

// Mostrar todos los empleados
empresa.mostrarEmpleados()
```

**Explicación:**

El código anterior define una clase `Empleado` para almacenar la información de los empleados, y una clase `Empresa` para gestionar los empleados.

La clase `Empresa` tiene una lista de empleados, y métodos para agregar, editar y eliminar empleados, así como para mostrar todos los empleados, los empleados de un departamento específico y la información de un empleado específico.

El código principal crea una instancia de la clase `Empresa`, agrega algunos empleados a la empresa, y luego muestra todos los empleados, los empleados del departamento de Ventas, la información del empleado Juan Pérez, y luego edita la información del empleado Juan Pérez y elimina al empleado Pedro López.

Finalmente, muestra todos los empleados de nuevo para mostrar los cambios.