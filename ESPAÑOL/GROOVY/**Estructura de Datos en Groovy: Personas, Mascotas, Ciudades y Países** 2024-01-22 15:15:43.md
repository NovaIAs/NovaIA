```groovy
// Definimos una clase 'Persona' con propiedades para el nombre, apellido y edad
class Persona {
    String nombre
    String apellido
    int edad
}

// Definimos una clase 'Mascota' con propiedades para el nombre, tipo y dueño
class Mascota {
    String nombre
    String tipo
    Persona dueño
}

// Definimos una clase 'Ciudad' con propiedades para el nombre, población y país
class Ciudad {
    String nombre
    int poblacion
    String pais
}

// Definimos una clase 'Pais' con propiedades para el nombre y capital
class Pais {
    String nombre
    String capital
}

// Creamos una lista de personas
def personas = [
    new Persona(nombre: 'Juan', apellido: 'Pérez', edad: 25),
    new Persona(nombre: 'María', apellido: 'Gómez', edad: 30),
    new Persona(nombre: 'Pedro', apellido: 'Rodríguez', edad: 35)
]

// Creamos una lista de mascotas
def mascotas = [
    new Mascota(nombre: 'Firulais', tipo: 'Perro', dueño: personas[0]),
    new Mascota(nombre: 'Michi', tipo: 'Gato', dueño: personas[1]),
    new Mascota(nombre: 'Pajarito', tipo: 'Canario', dueño: personas[2])
]

// Creamos una lista de ciudades
def ciudades = [
    new Ciudad(nombre: 'Madrid', poblacion: 3223000, pais: 'España'),
    new Ciudad(nombre: 'Barcelona', poblacion: 1620809, pais: 'España'),
    new Ciudad(nombre: 'Valencia', poblacion: 794282, pais: 'España')
]

// Creamos una lista de países
def paises = [
    new Pais(nombre: 'España', capital: 'Madrid'),
    new Pais(nombre: 'Francia', capital: 'París'),
    new Pais(nombre: 'Alemania', capital: 'Berlín')
]

// Imprimimos la lista de personas
println 'Personas:'
personas.each { persona ->
    println "${persona.nombre} ${persona.apellido} (${persona.edad} años)"
}

// Imprimimos la lista de mascotas
println '\nMascotas:'
mascotas.each { mascota ->
    println "${mascota.nombre} (${mascota.tipo}) es la mascota de ${mascota.dueño.nombre} ${mascota.dueño.apellido}"
}

// Imprimimos la lista de ciudades
println '\nCiudades:'
ciudades.each { ciudad ->
    println "${ciudad.nombre} (${ciudad.poblacion} habitantes) está en ${ciudad.pais}"
}

// Imprimimos la lista de países
println '\nPaíses:'
paises.each { pais ->
    println "${pais.nombre} (capital: ${pais.capital})"
}

// Consultamos la mascota de una persona específica
def persona = personas[0]
println "\nLa mascota de ${persona.nombre} ${persona.apellido} es ${persona.mascota.nombre}"

// Consultamos la ciudad donde vive una persona específica
println "\n${persona.nombre} ${persona.apellido} vive en ${persona.ciudad.nombre}"

// Consultamos el país donde se encuentra una ciudad específica
println "\n${ciudades[0].nombre} está en ${ciudades[0].pais.nombre}"
```

Explicación del código:

1. Definimos cuatro clases: 'Persona', 'Mascota', 'Ciudad' y 'Pais'. Cada clase tiene propiedades específicas relacionadas con su propósito.
2. Creamos listas de objetos para cada clase, utilizando constructores para inicializar las propiedades de cada objeto.
3. Imprimimos las listas de personas, mascotas, ciudades y países utilizando la función 'println'.
4. Realizamos consultas específicas sobre la mascota de una persona, la ciudad donde vive una persona y el país donde se encuentra una ciudad.

Este código es complejo y diferenciado, ya que se utilizan múltiples clases y objetos para representar diferentes entidades (personas, mascotas, ciudades y países), y se realizan consultas específicas sobre los objetos. El código es difícil de repetir, ya que se utilizan diferentes clases y objetos, y los objetos se relacionan entre sí de manera específica.