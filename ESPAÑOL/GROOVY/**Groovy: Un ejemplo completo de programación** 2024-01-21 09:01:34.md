```groovy
// Definición de variables

// Una lista de nombres de personas
def personas = ["Juan", "María", "Pedro", "Ana"]

// Un mapa de personas y sus edades
def edades = [Juan: 20, María: 22, Pedro: 24, Ana: 26]

// Una función que devuelve el nombre de una persona aleatoria de la lista
def obtenerPersonaAleatoria() {
    return personas[new Random().nextInt(personas.size())]
}

// Una función que devuelve la edad de una persona dada su nombre
def obtenerEdad(nombre) {
    return edades[nombre]
}

// Una función que saluda a una persona dada su nombre y edad
def saludar(nombre, edad) {
    println "Hola, $nombre! Tienes $edad años."
}

// Un bucle que saluda a cada persona de la lista
for (persona in personas) {
    def edad = obtenerEdad(persona)
    saludar(persona, edad)
}

// Una función que devuelve una lista de los nombres de las personas que son mayores de una edad determinada
def obtenerPersonasMayoresDe(edad) {
    return personas.findAll { it -> obtenerEdad(it) > edad }
}

// Una lista de los nombres de las personas que son mayores de 23 años
def personasMayoresDe23 = obtenerPersonasMayoresDe(23)

// Una función que imprime una lista de nombres de personas
def imprimirLista(lista) {
    lista.each { println it }
}

// Imprime la lista de nombres de las personas que son mayores de 23 años
imprimirLista(personasMayoresDe23)
```

Este código es un ejemplo de cómo utilizar las características del lenguaje Groovy para crear un programa complejo y diferenciado.

El código empieza definiendo una lista de nombres de personas, un mapa de personas y sus edades, y una serie de funciones que realizan diferentes tareas con estos datos.

El bucle `for` recorre la lista de nombres de personas y saluda a cada persona llamándola por su nombre y edad.

La función `obtenerPersonasMayoresDe()` devuelve una lista de los nombres de las personas que son mayores de una edad determinada.

La lista de nombres de las personas que son mayores de 23 años se imprime usando la función `imprimirLista()`.

Este código es un ejemplo de cómo utilizar Groovy para crear un programa complejo y diferenciado. El código es fácil de leer y entender, y utiliza las características del lenguaje para crear un programa eficiente y eficaz.