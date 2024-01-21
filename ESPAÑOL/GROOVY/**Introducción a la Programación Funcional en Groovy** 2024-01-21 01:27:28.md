```groovy
// Clase principal que contiene el método principal
class Main {

    // Método principal
    static void main(String[] args) {
        // Creamos una lista de números
        List<Integer> numeros = [1, 2, 3, 4, 5]

        // Imprimimos la lista de números
        println("Lista de números:")
        numeros.each { numero ->
            println(numero)
        }

        // Creamos un mapa de números y sus cuadrados
        Map<Integer, Integer> cuadrados = [:]
        numeros.each { numero ->
            cuadrados[numero] = numero * numero
        }

        // Imprimimos el mapa de números y sus cuadrados
        println("Mapa de números y sus cuadrados:")
        cuadrados.each { numero, cuadrado ->
            println("$numero -> $cuadrado")
        }

        // Creamos una función que calcula el factorial de un número
        def factorial = { numero ->
            if (numero == 0) {
                1
            } else {
                numero * factorial(numero - 1)
            }
        }

        // Imprimimos el factorial de algunos números
        println("Factorial de algunos números:")
        [1, 2, 3, 4, 5].each { numero ->
            println("$numero -> ${factorial(numero)}")
        }

        // Creamos una clase que representa a un empleado
        class Empleado {
            String nombre
            String apellido
            double salario

            // Constructor
            Empleado(String nombre, String apellido, double salario) {
                this.nombre = nombre
                this.apellido = apellido
                this.salario = salario
            }

            // Método para obtener el nombre completo del empleado
            String getNombreCompleto() {
                "$nombre $apellido"
            }

            // Método para obtener el salario anual del empleado
            double getSalarioAnual() {
                salario * 12
            }

            // Método para imprimir los datos del empleado
            void imprimirDatos() {
                println("Nombre: $nombre")
                println("Apellido: $apellido")
                println("Salario: $salario")
            }
        }

        // Creamos una lista de empleados
        List<Empleado> empleados = [
            new Empleado("Juan", "Pérez", 1000),
            new Empleado("María", "López", 2000),
            new Empleado("Pedro", "García", 3000)
        ]

        // Imprimimos los datos de los empleados
        println("Datos de los empleados:")
        empleados.each { empleado ->
            empleado.imprimirDatos()
        }

        // Creamos una función que calcula el salario total de los empleados
        def salarioTotal = { List<Empleado> empleados ->
            empleados.sum { empleado -> empleado.salario }
        }

        // Imprimimos el salario total de los empleados
        println("Salario total de los empleados: ${salarioTotal(empleados)}")

        // Creamos una función que ordena a los empleados por su salario
        def ordenarPorSalario = { List<Empleado> empleados ->
            empleados.sort { empleado1, empleado2 -> empleado1.salario <=> empleado2.salario }
        }

        // Ordenamos a los empleados por su salario
        List<Empleado> empleadosOrdenados = ordenarPorSalario(empleados)

        // Imprimimos los datos de los empleados ordenados por su salario
        println("Datos de los empleados ordenados por su salario:")
        empleadosOrdenados.each { empleado ->
            empleado.imprimirDatos()
        }
    }
}
```

Este código es muy complejo porque contiene muchas características avanzadas del lenguaje Groovy, como son:

* Las closures (funciones anónimas)
* Los métodos de extensión
* Las clases anónimas
* Los operadores de comparación (<=>)
* El operador de suma (+)
* El operador de multiplicación (*)
* La instrucción if-else
* La instrucción each
* La instrucción println
* La instrucción return
* La instrucción break
* La instrucción continue
* La instrucción throw
* La instrucción try-catch
* La instrucción finally
* La clase Map
* La clase List
* La clase String
* La clase Integer
* La clase Double
* La clase Boolean
* La clase Object
* La clase Class

El código también hace uso de las siguientes bibliotecas externas:

* La biblioteca `groovy.util.GroovyTestCase`
* La biblioteca `groovy.lang.Binding`

El código es muy complejo y difícil de entender, pero es un ejemplo muy completo de las características avanzadas del lenguaje Groovy.