```groovy
// Este es un código complejo en Groovy que realiza múltiples tareas y demuestra diferentes características del lenguaje.

// Definir una lista de números.
def numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

// Utilizar el método each para iterar sobre la lista y sumar cada número.
def suma = 0
numeros.each {
    suma += it
}

// Imprimir el resultado de la suma.
println "La suma de los números es: ${suma}"

// Definir un mapa para almacenar información sobre personas.
def personas = [:]
personas["Juan"] = ["edad": 25, "profesión": "Ingeniero"]
personas["María"] = ["edad": 30, "profesión": "Médica"]
personas["Pedro"] = ["edad": 35, "profesión": "Abogado"]

// Utilizar el método each para iterar sobre el mapa y mostrar la información de cada persona.
personas.each { nombre, datos ->
    println "Nombre: ${nombre}"
    println "Edad: ${datos["edad"]}"
    println "Profesión: ${datos["profesión"]}"
    println ""
}

// Definir una clase para representar un animal.
class Animal {
    String nombre
    String especie

    Animal(String nombre, String especie) {
        this.nombre = nombre
        this.especie = especie
    }

    String ladrar() {
        return "Woof woof!"
    }
}

// Crear un objeto de la clase Animal.
def perro = new Animal("Firulais", "Perro")

// Llamar al método ladrar del objeto perro.
println perro.ladrar()

// Definir una interfaz para representar un vehículo.
interface Vehiculo {
    String acelerar()
    String frenar()
}

// Definir una clase para representar un automóvil.
class Automovil implements Vehiculo {
    String acelerar() {
        return "El automóvil acelera."
    }

    String frenar() {
        return "El automóvil frena."
    }
}

// Crear un objeto de la clase Automóvil.
def automóvil = new Automovil()

// Llamar a los métodos acelerar y frenar del objeto automóvil.
println automóvil.acelerar()
println automóvil.frenar()

// Definir una función para calcular el área de un círculo.
def calcularAreaCirculo(double radio) {
    return Math.PI * radio ** 2
}

// Llamar a la función calcularAreaCirculo con un radio de 5.
println "El área del círculo con radio 5 es: ${calcularAreaCirculo(5)}"
```

Explicación del código:

1. Se define una lista de números y se calcula su suma utilizando el método `each`.
2. Se define un mapa para almacenar información sobre personas y se itera sobre el mapa para mostrar la información de cada persona.
3. Se define una clase `Animal` con dos propiedades (`nombre` y `especie`) y un método `ladrar`. Se crea un objeto de la clase `Animal` y se llama al método `ladrar` del objeto.
4. Se define una interfaz `Vehiculo` con dos métodos (`acelerar` y `frenar`). Se define una clase `Automovil` que implementa la interfaz `Vehiculo`. Se crea un objeto de la clase `Automovil` y se llaman a los métodos `acelerar` y `frenar` del objeto.
5. Se define una función `calcularAreaCirculo` que calcula el área de un círculo dado su radio. Se llama a la función con un radio de 5 y se imprime el resultado.