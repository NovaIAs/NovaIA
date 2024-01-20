```groovy
// Inicialización de variables
def nombres = ['Juan', 'María', 'Pedro', 'Ana', 'Luis']
def edades = [20, 25, 30, 35, 40]
def ciudades = ['Madrid', 'Barcelona', 'Valencia', 'Sevilla', 'Zaragoza']

// Creación de una lista de objetos Persona
def personas = []
nombres.eachWithIndex { nombre, i ->
    personas << [nombre: nombre, edad: edades[i], ciudad: ciudades[i]]
}

// Impresión de la lista de objetos Persona
personas.each { persona ->
    println "Nombre: ${persona.nombre}, Edad: ${persona.edad}, Ciudad: ${persona.ciudad}"
}

// Creación de un mapa de nombres y edades
def mapaNombresEdades = [:]
nombres.eachWithIndex { nombre, i ->
    mapaNombresEdades[nombre] = edades[i]
}

// Impresión del mapa de nombres y edades
mapaNombresEdades.each { nombre, edad ->
    println "Nombre: ${nombre}, Edad: ${edad}"
}

// Creación de una lista de objetos FiltroPersona
def filtroPersonas = []
personas.each { persona ->
    if (persona.edad > 25) {
        filtroPersonas << persona
    }
}

// Impresión de la lista de objetos FiltroPersona
filtroPersonas.each { persona ->
    println "Nombre: ${persona.nombre}, Edad: ${persona.edad}, Ciudad: ${persona.ciudad}"
}

// Creación de una lista de nombres de las personas que viven en Madrid
def nombresMadrid = []
personas.each { persona ->
    if (persona.ciudad == 'Madrid') {
        nombresMadrid << persona.nombre
    }
}

// Impresión de la lista de nombres de las personas que viven en Madrid
nombresMadrid.each { nombre ->
    println nombre
}

// Creación de un mapa de nombres y edades de las personas que viven en Barcelona
def mapaNombresEdadesBarcelona = [:]
personas.each { persona ->
    if (persona.ciudad == 'Barcelona') {
        mapaNombresEdadesBarcelona[persona.nombre] = persona.edad
    }
}

// Impresión del mapa de nombres y edades de las personas que viven en Barcelona
mapaNombresEdadesBarcelona.each { nombre, edad ->
    println "Nombre: ${nombre}, Edad: ${edad}"
}
```

Explicación del código:

* Se inicializan las variables `nombres`, `edades` y `ciudades` con valores de ejemplo.
* Se crea una lista de objetos `Persona` con los datos de los nombres, edades y ciudades.
* Se imprime la lista de objetos `Persona`.
* Se crea un mapa de nombres y edades a partir de la lista de objetos `Persona`.
* Se imprime el mapa de nombres y edades.
* Se crea una lista de objetos `FiltroPersona` con las personas que tienen más de 25 años.
* Se imprime la lista de objetos `FiltroPersona`.
* Se crea una lista de nombres de las personas que viven en Madrid.
* Se imprime la lista de nombres de las personas que viven en Madrid.
* Se crea un mapa de nombres y edades de las personas que viven en Barcelona.
* Se imprime el mapa de nombres y edades de las personas que viven en Barcelona.