```groovy
// Definición de una clase
class Persona {
  String nombre
  int edad

  // Constructor de la clase
  Persona(String nombre, int edad) {
    this.nombre = nombre
    this.edad = edad
  }

  // Método para obtener el nombre de la persona
  String getNombre() {
    return nombre
  }

  // Método para obtener la edad de la persona
  int getEdad() {
    return edad
  }

  // Método para imprimir los datos de la persona
  void imprimirDatos() {
    println "Nombre: $nombre, Edad: $edad"
  }
}

// Definición de una clase que extiende a Persona
class Empleado extends Persona {
  String puesto
  double salario

  // Constructor de la clase
  Empleado(String nombre, int edad, String puesto, double salario) : super(nombre, edad) {
    this.puesto = puesto
    this.salario = salario
  }

  // Método para obtener el puesto del empleado
  String getPuesto() {
    return puesto
  }

  // Método para obtener el salario del empleado
  double getSalario() {
    return salario
  }

  // Método para imprimir los datos del empleado
  void imprimirDatos() {
    super.imprimirDatos()
    println "Puesto: $puesto, Salario: $salario"
  }
}

// Creación de una lista de personas
def personas = [
  new Persona("Juan", 25),
  new Persona("María", 30),
  new Persona("Pedro", 35)
]

// Creación de una lista de empleados
def empleados = [
  new Empleado("Ana", 28, "Gerente", 3000.0),
  new Empleado("Luis", 32, "Ingeniero", 2500.0),
  new Empleado("Sara", 26, "Secretaria", 2000.0)
]

// Impresión de los datos de las personas
println "Personas:"
personas.each { persona ->
  persona.imprimirDatos()
}

// Impresión de los datos de los empleados
println "Empleados:"
empleados.each { empleado ->
  empleado.imprimirDatos()
}

// Búsqueda de una persona por su nombre
def personaBuscada = personas.find { persona -> persona.nombre == "Juan" }
println "Persona encontrada: $personaBuscada"

// Búsqueda de un empleado por su puesto
def empleadoBuscado = empleados.find { empleado -> empleado.puesto == "Ingeniero" }
println "Empleado encontrado: $empleadoBuscado"

// Ordenamiento de las personas por edad
def personasOrdenadasPorEdad = personas.sort { persona1, persona2 -> persona1.edad <=> persona2.edad }
println "Personas ordenadas por edad:"
personasOrdenadasPorEdad.each { persona ->
  persona.imprimirDatos()
}

// Ordenamiento de los empleados por salario
def empleadosOrdenadosPorSalario = empleados.sort { empleado1, empleado2 -> empleado1.salario <=> empleado2.salario }
println "Empleados ordenados por salario:"
empleadosOrdenadosPorSalario.each { empleado ->
  empleado.imprimirDatos()
}
```

Este código crea dos clases, `Persona` y `Empleado`, que representan a personas y empleados respectivamente. A continuación, crea una lista de personas y una lista de empleados, e imprime sus datos. Luego, busca una persona por su nombre y un empleado por su puesto, y los imprime. Finalmente, ordena las personas por edad y los empleados por salario, e imprime los resultados.