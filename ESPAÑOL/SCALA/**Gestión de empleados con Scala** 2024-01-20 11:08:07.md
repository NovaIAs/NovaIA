```scala
// Definición de una clase con los datos de los empleados
class Empleado(val nombre: String, val apellido: String, val sueldo: Int) {
  // Método para obtener el nombre completo del empleado
  def getNombreCompleto: String = s"$nombre $apellido"
}

// Definición de una clase para gestionar la lista de empleados
class ListaEmpleados {
  // Lista mutable de empleados
  private var empleados = List[Empleado]()

  // Método para añadir un empleado a la lista
  def añadirEmpleado(empleado: Empleado): Unit = {
    empleados = empleado :: empleados
  }

  // Método para obtener la lista de empleados
  def getEmpleados: List[Empleado] = empleados

  // Método para obtener el empleado con el sueldo más alto
  def getEmpleadoSueldoMax: Empleado = {
    empleados.maxBy(_.sueldo)
  }

  // Método para obtener la lista de empleados ordenados por apellido
  def getEmpleadosOrdenadosPorApellido: List[Empleado] = {
    empleados.sortBy(_.apellido)
  }
}

// Creación de una instancia de la clase ListaEmpleados
val listaEmpleados = new ListaEmpleados

// Añadiendo algunos empleados a la lista
listaEmpleados.añadirEmpleado(new Empleado("Juan", "García", 3000))
listaEmpleados.añadirEmpleado(new Empleado("María", "Pérez", 2500))
listaEmpleados.añadirEmpleado(new Empleado("Pedro", "López", 4000))

// Obteniendo la lista de empleados
val empleados = listaEmpleados.getEmpleados

// Iterando sobre la lista de empleados y mostrando su nombre completo
println("Lista de empleados:")
empleados.foreach { empleado => println(empleado.getNombreCompleto) }

// Obteniendo el empleado con el sueldo más alto
val empleadoSueldoMax = listaEmpleados.getEmpleadoSueldoMax
println(s"El empleado con el sueldo más alto es: ${empleadoSueldoMax.getNombreCompleto}")

// Obteniendo la lista de empleados ordenados por apellido
val empleadosOrdenadosPorApellido = listaEmpleados.getEmpleadosOrdenadosPorApellido
println("Lista de empleados ordenados por apellido:")
empleadosOrdenadosPorApellido.foreach { empleado => println(empleado.getNombreCompleto) }
```

Explicación del código:

1. Definimos una clase llamada `Empleado` que tiene los siguientes atributos: `nombre`, `apellido` y `sueldo`.
2. Definimos una clase llamada `ListaEmpleados` que tiene los siguientes métodos:
    * `añadirEmpleado`: Añade un empleado a la lista.
    * `getEmpleados`: Obtiene la lista de empleados.
    * `getEmpleadoSueldoMax`: Obtiene el empleado con el sueldo más alto.
    * `getEmpleadosOrdenadosPorApellido`: Obtiene la lista de empleados ordenados por apellido.
3. Creamos una instancia de la clase `ListaEmpleados` llamada `listaEmpleados`.
4. Añadimos algunos empleados a la lista utilizando el método `añadirEmpleado`.
5. Obtenemos la lista de empleados utilizando el método `getEmpleados`.
6. Iteramos sobre la lista de empleados y mostramos su nombre completo.
7. Obtenemos el empleado con el sueldo más alto utilizando el método `getEmpleadoSueldoMax`.
8. Obtenemos la lista de empleados ordenados por apellido utilizando el método `getEmpleadosOrdenadosPorApellido`.
9. Mostramos la lista de empleados ordenados por apellido.