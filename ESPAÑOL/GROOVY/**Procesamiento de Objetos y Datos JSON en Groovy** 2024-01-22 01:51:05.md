```groovy
// Importamos las librerías necesarias
import groovy.json.JsonSlurper
import groovy.json.JsonOutput
import groovy.transform.ToString

// Definimos una clase para representar a un empleado
@ToString(includes = 'nombre, apellidos, salario')
class Empleado {

    String nombre
    String apellidos
    BigDecimal salario

    // Constructor con parámetros
    Empleado(String nombre, String apellidos, BigDecimal salario) {
        this.nombre = nombre
        this.apellidos = apellidos
        this.salario = salario
    }

}

// Definimos una lista de empleados
List<Empleado> empleados = [
    new Empleado('Juan', 'García', new BigDecimal('1000')),
    new Empleado('María', 'Pérez', new BigDecimal('2000')),
    new Empleado('Pedro', 'López', new BigDecimal('3000')),
]

// Serializamos la lista de empleados a JSON
String json = JsonOutput.toJson(empleados)

// Mostramos el JSON por consola
println json

// Leemos el JSON y lo deserializamos a una lista de empleados
List<Empleado> empleadosDeserializados = new JsonSlurper().parseText(json)

// Mostramos los datos de los empleados deserializados
empleadosDeserializados.each { empleado ->
    println empleado
}

// Filtramos los empleados que ganen más de 2000
List<Empleado> empleadosConSalarioMayorA2000 = empleados.findAll { empleado -> empleado.salario > new BigDecimal('2000') }

// Mostramos los datos de los empleados filtrados
empleadosConSalarioMayorA2000.each { empleado ->
    println empleado
}

// Agrupamos los empleados por su salario
Map<BigDecimal, List<Empleado>> empleadosAgrupadosPorSalario = empleados.groupBy { empleado -> empleado.salario }

// Mostramos los datos de los empleados agrupados
empleadosAgrupadosPorSalario.each { salario, empleados ->
    println "Salario: ${salario}"
    empleados.each { empleado ->
        println "\t${empleado}"
    }
}

// Ordenamos los empleados por su salario en orden descendente
List<Empleado> empleadosOrdenadosPorSalarioDescendente = empleados.sort { empleado1, empleado2 -> empleado2.salario <=> empleado1.salario }

// Mostramos los datos de los empleados ordenados
empleadosOrdenadosPorSalarioDescendente.each { empleado ->
    println empleado
}
```

Explicación del código:

1. Importamos las librerías necesarias.


2. Definimos una clase para representar a un empleado.


3. Definimos una lista de empleados.


4. Serializamos la lista de empleados a JSON.


5. Mostramos el JSON por consola.


6. Leemos el JSON y lo deserializamos a una lista de empleados.


7. Mostramos los datos de los empleados deserializados.


8. Filtramos los empleados que ganen más de 2000.


9. Mostramos los datos de los empleados filtrados.


10. Agrupamos los empleados por su salario.


11. Mostramos los datos de los empleados agrupados.


12. Ordenamos los empleados por su salario en orden descendente.


13. Mostramos los datos de los empleados ordenados.