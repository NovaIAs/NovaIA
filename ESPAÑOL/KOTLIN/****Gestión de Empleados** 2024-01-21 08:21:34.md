```kotlin
// Clase para representar un empleado
class Empleado(val nombre: String, val salario: Double) {
    fun obtenerNombre(): String {
        return nombre
    }

    fun obtenerSalario(): Double {
        return salario
    }

    override fun toString(): String {
        return "Empleado(nombre='$nombre', salario=$salario)"
    }
}

// Función para obtener la lista de empleados
fun obtenerEmpleados(): List<Empleado> {
    return listOf(
        Empleado("Juan Pérez", 1000.0),
        Empleado("María López", 1200.0),
        Empleado("Pedro Sánchez", 1500.0)
    )
}

// Función para calcular el salario total de los empleados
fun calcularSalarioTotal(empleados: List<Empleado>): Double {
    return empleados.sumByDouble { it.obtenerSalario() }
}

// Función para obtener el empleado con el salario más alto
fun obtenerEmpleadoConSalarioMasAlto(empleados: List<Empleado>): Empleado? {
    return empleados.maxByOrNull { it.obtenerSalario() }
}

// Función para imprimir los datos de un empleado
fun imprimirEmpleado(empleado: Empleado) {
    println("Nombre: ${empleado.obtenerNombre()}")
    println("Salario: ${empleado.obtenerSalario()}")
}

// Función principal
fun main(args: Array<String>) {
    // Obtener la lista de empleados
    val empleados = obtenerEmpleados()

    // Imprimir los datos de los empleados
    empleados.forEach { imprimirEmpleado(it) }

    // Calcular el salario total de los empleados
    val salarioTotal = calcularSalarioTotal(empleados)

    // Imprimir el salario total
    println("Salario total: $salarioTotal")

    // Obtener el empleado con el salario más alto
    val empleadoConSalarioMasAlto = obtenerEmpleadoConSalarioMasAlto(empleados)

    // Imprimir los datos del empleado con el salario más alto
    if (empleadoConSalarioMasAlto != null) {
        println("Empleado con el salario más alto:")
        imprimirEmpleado(empleadoConSalarioMasAlto)
    } else {
        println("No hay empleados")
    }
}
```

Explicación del código:

* La clase `Empleado` representa a un empleado con su nombre y salario.
* La función `obtenerEmpleados()` devuelve una lista de empleados.
* La función `calcularSalarioTotal()` calcula el salario total de los empleados.
* La función `obtenerEmpleadoConSalarioMasAlto()` devuelve el empleado con el salario más alto.
* La función `imprimirEmpleado()` imprime los datos de un empleado.
* La función `main()` es la función principal del programa.

El código crea una lista de empleados, calcula el salario total de los empleados, obtiene el empleado con el salario más alto e imprime los datos de los empleados.