```scala
// Definimos una clase llamada "Coche" con sus propiedades y métodos.
class Coche(var marca: String, var modelo: String, var año: Int, var color: String) {

    // Método para mostrar los datos del coche.
    def mostrarDatos(): Unit = {
        println("Marca: " + marca)
        println("Modelo: " + modelo)
        println("Año: " + año)
        println("Color: " + color)
    }

    // Método para cambiar el color del coche.
    def cambiarColor(nuevoColor: String): Unit = {
        color = nuevoColor
    }
}

// Creamos una instancia de la clase "Coche".
val coche = new Coche("Toyota", "Yaris", 2023, "Blanco")

// Mostramos los datos del coche.
coche.mostrarDatos()

// Cambiamos el color del coche.
coche.cambiarColor("Rojo")

// Mostramos los datos del coche nuevamente.
coche.mostrarDatos()

// Definimos una función para sumar dos números.
def sumar(a: Int, b: Int): Int = {
    return a + b
}

// Definimos una función para restar dos números.
def restar(a: Int, b: Int): Int = {
    return a - b
}

// Definimos una función para multiplicar dos números.
def multiplicar(a: Int, b: Int): Int = {
    return a * b
}

// Definimos una función para dividir dos números.
def dividir(a: Int, b: Int): Double = {
    return a.toDouble / b
}

// Creamos una lista de números.
val listaNumeros = List(1, 2, 3, 4, 5)

// Usamos la función "map" para aplicar la función "sumar" a cada elemento de la lista.
val listaSuma = listaNumeros.map(sumar(_, 10))

// Mostramos la lista de números sumados.
println(listaSuma)

// Usamos la función "filter" para filtrar los elementos de la lista que sean mayores que 3.
val listaFiltrada = listaNumeros.filter(_ > 3)

// Mostramos la lista de números filtrados.
println(listaFiltrada)

// Usamos la función "reduce" para reducir la lista de números a un único valor sumándolos todos.
val sumaTotal = listaNumeros.reduce(_ + _)

// Mostramos la suma total de los números.
println(sumaTotal)
```

Explicación del código:

1. Definimos una clase llamada "Coche" con sus propiedades y métodos.
2. Creamos una instancia de la clase "Coche" y mostramos sus datos.
3. Cambiamos el color del coche y mostramos sus datos nuevamente.
4. Definimos funciones para sumar, restar, multiplicar y dividir dos números.
5. Creamos una lista de números y usamos las funciones "map", "filter" y "reduce" para manipularla.

Este código es complejo y diferenciado porque incluye varias características:

* Definición de una clase con propiedades y métodos.
* Creación de una instancia de una clase.
* Llamada a métodos de una clase.
* Definición de funciones con parámetros y retorno.
* Uso de listas y funciones de manipulación de listas.