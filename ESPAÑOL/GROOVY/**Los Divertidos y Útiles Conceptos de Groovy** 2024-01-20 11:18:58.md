```groovy
// Definición de una función de orden superior y de una closure en Groovy.
def sumador = { int num1, int num2 -> num1 + num2 }
println sumador(3, 7) // Imprime 10

// Uso de expresiones lambda en Groovy.
// La expresión lambda define una closure que recibe dos parámetros y devuelve su suma.
def sumadorLambda = { num1, num2 -> num1 + num2 }
println sumadorLambda(5, 9) // Imprime 14

// Declaración de una clase en Groovy con una propiedad y un método.
class Persona {
    String nombre
    String apellido

    String nombreCompleto() { nombre + " " + apellido }
}

// Creación de una instancia de la clase Persona y llamada al método nombreCompleto.
def persona = new Persona(nombre: "Juan", apellido: "Pérez")
println persona.nombreCompleto() // Imprime "Juan Pérez"

// Utilización de la palabra clave `each` para iterar sobre una lista de elementos.
def numeros = [1, 2, 3, 4, 5]
numeros.each { println "Número: $it" }

// Uso de la palabra clave `with` para encapsular una instancia en un bloque de código.
def nombre = "María"
with(nombre) {
    println "${toUpperCase()}" // Imprime "MARÍA"
    println "${toLowerCase()}" // Imprime "maría"
}

// Utilización de la palabra clave `assert` para realizar comprobaciones durante la ejecución.
assert 1 + 1 == 2 // Comprueba si la suma de 1 y 1 es igual a 2, sino lanza una excepción.
println "Operación exitosa" // Se imprimirá si la comprobación anterior es verdadera.

// Uso de la palabra clave `try` para manejar excepciones en Groovy.
try {
    def resultado = 1 / 0 // Intenta dividir 1 entre 0, lo que lanzará una excepción.
} catch (ArithmeticException e) {
    println "Error: ${e.message}" // Imprime "Error: / by zero"
} finally {
    println "Bloque finally siempre se ejecuta" // Se imprimirá siempre, independientemente de si hay una excepción o no.
}
```

Explicación del código:

- Se definen una función de orden superior y una closure en Groovy, y se demuestra su uso.
- Se utiliza una expresión lambda para definir una closure que realiza la suma de dos números.
- Se declara una clase en Groovy y se muestra cómo crear una instancia y llamar a un método de esa clase.
- Se utiliza la palabra clave `each` para iterar sobre una lista de elementos.
- Se usa la palabra clave `with` para encapsular una instancia en un bloque de código.
- Se usa la palabra clave `assert` para realizar comprobaciones durante la ejecución.
- Se utiliza la palabra clave `try` para manejar excepciones en Groovy.