```groovy
// Definimos una clase llamada "Persona" con propiedades y métodos.
class Persona {
    // Las propiedades son variables que pertenecen a la clase.
    String nombre
    int edad
    String ocupacion

    // Los métodos son funciones que pueden ser llamadas desde la clase.
    def saludar() {
        // El método "saludar" imprime un mensaje con el nombre de la persona.
        println "Hola, mi nombre es ${nombre}."
    }

    def trabajar() {
        // El método "trabajar" imprime un mensaje con la ocupación de la persona.
        println "Soy un ${ocupacion}."
    }

    // El método "toString" se llama cuando se imprime un objeto.
    @Override
    String toString() {
        // El método "toString" devuelve una cadena con el nombre y la edad de la persona.
        return "Nombre: ${nombre}, Edad: ${edad}"
    }
}

// Creamos una lista de personas.
def personas = [
    new Persona(nombre: "Juan", edad: 25, ocupacion: "Ingeniero"),
    new Persona(nombre: "María", edad: 30, ocupacion: "Médica"),
    new Persona(nombre: "Pedro", edad: 35, ocupacion: "Profesor")
]

// Iteramos sobre la lista de personas y llamamos al método "saludar" de cada persona.
personas.each { persona ->
    persona.saludar()
}

// Imprimimos la lista de personas usando el método "toString" de cada persona.
println personas

// Creamos una función llamada "calcularPromedioEdad" que calcula la edad promedio de una lista de personas.
def calcularPromedioEdad(personas) {
    // Sumamos las edades de todas las personas.
    int sumaEdades = 0
    personas.each { persona ->
        sumaEdades += persona.edad
    }

    // Dividimos la suma de las edades por el número de personas.
    int promedioEdad = sumaEdades / personas.size()

    // Devolvemos el promedio de edad.
    return promedioEdad
}

// Llamamos a la función "calcularPromedioEdad" con la lista de personas.
int promedioEdad = calcularPromedioEdad(personas)

// Imprimimos el promedio de edad.
println "Promedio de edad: ${promedioEdad}"
```

Explicación del código:

* La clase `Persona` define las propiedades `nombre`, `edad` y `ocupacion`, y los métodos `saludar`, `trabajar` y `toString`.
* La lista `personas` contiene tres objetos de tipo `Persona`.
* El ciclo `personas.each { persona -> persona.saludar() }` llama al método `saludar` de cada persona en la lista.
* La instrucción `println personas` imprime la lista de personas usando el método `toString` de cada persona.
* La función `calcularPromedioEdad` calcula la edad promedio de una lista de personas.
* La instrucción `int promedioEdad = calcularPromedioEdad(personas)` llama a la función `calcularPromedioEdad` con la lista de personas.
* La instrucción `println "Promedio de edad: ${promedioEdad}"` imprime el promedio de edad.