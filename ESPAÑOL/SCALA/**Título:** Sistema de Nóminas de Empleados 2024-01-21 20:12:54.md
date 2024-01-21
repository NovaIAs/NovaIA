```scala
// Clase Persona que representa a una persona con nombre, apellido y edad
class Persona(nombre: String, apellido: String, edad: Int) {
  // Definir los atributos de la clase
  var nombre = nombre
  var apellido = apellido
  var edad = edad

  // Definir un metodo para saludar
  def saludar(): String = {
    return "Hola, mi nombre es " + nombre + " " + apellido + " y tengo " + edad + " años."
  }
}

// Clase Empleado que hereda de la clase Persona y añade el atributo salario
class Empleado(nombre: String, apellido: String, edad: Int, salario: Double) extends Persona(nombre, apellido, edad) {
  // Definir el atributo salario
  var salario = salario

  // Sobrescribir el metodo saludar para incluir el salario
  override def saludar(): String = {
    return super.saludar() + " Y mi salario es " + salario + " euros."
  }
}

// Clase Empresa que tiene una lista de empleados
class Empresa(nombre: String, empleados: List[Empleado]) {
  // Definir los atributos de la clase
  var nombre = nombre
  var empleados = empleados

  // Definir un metodo para calcular el salario total de la empresa
  def calcularSalarioTotal(): Double = {
    var salarioTotal = 0.0
    for (empleado <- empleados) {
      salarioTotal += empleado.salario
    }
    return salarioTotal
  }

  // Definir un metodo para imprimir los datos de la empresa y sus empleados
  def imprimirDatos(): Unit = {
    println("Nombre de la empresa: " + nombre)
    println("Empleados:")
    for (empleado <- empleados) {
      println(empleado.saludar())
    }
    println("Salario total de la empresa: " + calcularSalarioTotal() + " euros")
  }
}

// Crear una instancia de la clase Empresa
val empresa = new Empresa("Acme Corporation", List(
  new Empleado("Juan", "Pérez", 30, 1000.0),
  new Empleado("Ana", "García", 25, 1500.0),
  new Empleado("Pedro", "López", 40, 2000.0)
))

// Imprimir los datos de la empresa
empresa.imprimirDatos()
```

Explicación del código:

* La clase Persona representa a una persona con nombre, apellido y edad.
* La clase Empleado hereda de la clase Persona y añade el atributo salario.
* La clase Empresa tiene una lista de empleados.
* La clase Empresa tiene un método para calcular el salario total de la empresa.
* La clase Empresa tiene un método para imprimir los datos de la empresa y sus empleados.
* Se crea una instancia de la clase Empresa con tres empleados.
* Se imprimen los datos de la empresa.