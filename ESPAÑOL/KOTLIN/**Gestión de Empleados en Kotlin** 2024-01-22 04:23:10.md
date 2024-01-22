```kotlin
// Definición de clase de Empleado
class Empleado(nombre: String, apellidos: String, salario: Double) {
    // Propiedades del empleado
    var nombre: String = nombre
    var apellidos: String = apellidos
    var salario: Double = salario

    // Método para obtener el nombre completo del empleado
    fun obtenerNombreCompleto(): String {
        return "$nombre $apellidos"
    }

    // Método para calcular el salario anual del empleado
    fun calcularSalarioAnual(): Double {
        return salario * 12
    }

    // Método para imprimir el nombre completo y el salario anual del empleado
    fun imprimirInformacion() {
        println("Nombre completo: $nombre $apellidos")
        println("Salario anual: ${calcularSalarioAnual()}")
    }
}

// Creación de una lista de empleados
val empleados = mutableListOf(
    Empleado("Juan", "Perez", 2500.0),
    Empleado("María", "García", 3200.0),
    Empleado("Pedro", "López", 4000.0)
)

// Bucle for para recorrer la lista de empleados
for (empleado in empleados) {
    // Impresión de la información de cada empleado
    empleado.imprimirInformacion()
    println()
}

// Utilizando la función filter para obtener una lista de empleados con un salario mayor a 3000
val empleadosConSalarioMayorA3000 = empleados.filter { empleado -> empleado.salario > 3000.0 }

// Impresión de la información de los empleados con salario mayor a 3000
println("Empleados con salario mayor a 3000:")
for (empleado in empleadosConSalarioMayorA3000) {
    empleado.imprimirInformacion()
    println()
}

// Utilizando la función map para obtener una lista de los nombres completos de los empleados
val nombresCompletos = empleados.map { empleado -> empleado.obtenerNombreCompleto() }

// Impresión de la lista de nombres completos de los empleados
println("Nombres completos de los empleados:")
for (nombreCompleto in nombresCompletos) {
    println(nombreCompleto)
}

// Utilizando la función reduce para calcular el salario total de todos los empleados
val salarioTotal = empleados.reduce { acumulador, empleado -> acumulador + empleado.salario }

// Impresión del salario total de todos los empleados
println("Salario total de todos los empleados: $salarioTotal")
```

Explicación del código:

* Se define una clase Empleado con las propiedades nombre, apellidos y salario.
* Se crean los métodos obtenerNombreCompleto(), calcularSalarioAnual() e imprimirInformacion() para obtener el nombre completo, el salario anual y la información del empleado, respectivamente.
* Se crea una lista de empleados con tres empleados.
* Se recorre la lista de empleados y se imprime la información de cada empleado.
* Se utiliza la función filter para obtener una lista de empleados con un salario mayor a 3000.
* Se imprime la información de los empleados con salario mayor a 3000.
* Se utiliza la función map para obtener una lista de los nombres completos de los empleados.
* Se imprime la lista de nombres completos de los empleados.
* Se utiliza la función reduce para calcular el salario total de todos los empleados.
* Se imprime el salario total de todos los empleados.