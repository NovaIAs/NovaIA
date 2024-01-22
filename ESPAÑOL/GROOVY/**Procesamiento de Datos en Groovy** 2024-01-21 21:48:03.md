```groovy
//Definir una clase llamada `Persona` con propiedades `nombre` y `edad`.
class Persona {
    String nombre
    int edad
}

//Definir una lista de `Personas`.
def personas = []

//Añadir algunas personas a la lista.
personas << new Persona(nombre: "Juan", edad: 21)
personas << new Persona(nombre: "María", edad: 25)
personas << new Persona(nombre: "Pedro", edad: 30)

//Filtrar la lista de personas por edad.
def personasMayoresDe25 = personas.findAll { it.edad > 25 }

//Imprimir los nombres de las personas mayores de 25 años.
personasMayoresDe25.each { println it.nombre }

//Crear un mapa con los nombres de las personas como claves y sus edades como valores.
def mapaPersonas = [:]
personas.each { mapaPersonas[it.nombre] = it.edad }

//Imprimir el nombre y la edad de la persona más joven.
def personaMasJoven = personas.min { it.edad }
println "Persona más joven: ${personaMasJoven.nombre} (${personaMasJoven.edad} años)"

//Imprimir el nombre y la edad de la persona más mayor.
def personaMasMayor = personas.max { it.edad }
println "Persona más mayor: ${personaMasMayor.nombre} (${personaMasMayor.edad} años)"

//Crear una función que calcule la edad media de las personas.
def edadMedia = {
    def sumaEdades = 0
    personas.each { sumaEdades += it.edad }
    sumaEdades / personas.size()
}

//Imprimir la edad media de las personas.
println "Edad media: ${edadMedia()} años"

//Crear una función que agrupe a las personas por edad.
def agruparPorEdad = {
    def grupos = [:]
    personas.each { persona ->
        def grupo = grupos[persona.edad] ?: []
        grupo << persona
        grupos[persona.edad] = grupo
    }
    grupos
}

//Imprimir los grupos de personas agrupadas por edad.
agruparPorEdad().each { edad, grupo ->
    println "Grupo edad ${edad} años:"
    grupo.each { println "\t${it.nombre} (${it.edad} años)" }
}

//Crear una clase llamada `Coche` con propiedades `marca`, `modelo` y `año`.
class Coche {
    String marca
    String modelo
    int año
}

//Definir una lista de `Coches`.
def coches = []

//Añadir algunos coches a la lista.
coches << new Coche(marca: "Toyota", modelo: "Yaris", año: 2020)
coches << new Coche(marca: "BMW", modelo: "Serie 3", año: 2021)
coches << new Coche(marca: "Mercedes-Benz", modelo: "Clase C", año: 2022)

//Filtrar la lista de coches por marca y año.
def cochesToyota2020 = coches.findAll { it.marca == "Toyota" && it.año == 2020 }

//Imprimir los modelos de los coches Toyota del año 2020.
cochesToyota2020.each { println it.modelo }

//Crear un mapa con las marcas de los coches como claves y sus modelos como valores.
def mapaCoches = [:]
coches.each { mapaCoches[it.marca] = it.modelo }

//Imprimir la marca y el modelo del coche más antiguo.
def cocheMasAntiguo = coches.min { it.año }
println "Coche más antiguo: ${cocheMasAntiguo.marca} ${cocheMasAntiguo.modelo} (${cocheMasAntiguo.año})"

//Imprimir la marca y el modelo del coche más moderno.
def cocheMasModerno = coches.max { it.año }
println "Coche más moderno: ${cocheMasModerno.marca} ${cocheMasModerno.modelo} (${cocheMasModerno.año})"

//Crear una función que calcule la edad media de los coches.
def edadMediaCoches = {
    def sumaEdades = 0
    coches.each { sumaEdades += (2023 - it.año) }
    sumaEdades / coches.size()
}

//Imprimir la edad media de los coches.
println "Edad media coches: ${edadMediaCoches()} años"

//Crear una función que agrupe a los coches por marca.
def agruparPorMarca = {
    def grupos = [:]
    coches.each { coche ->
        def grupo = grupos[coche.marca] ?: []
        grupo << coche
        grupos[coche.marca] = grupo
    }
    grupos
}

//Imprimir los grupos de coches agrupados por marca.
agruparPorMarca().each { marca, grupo ->
    println "Grupo marca ${marca}:"
    grupo.each { println "\t${it.modelo} (${it.año})" }
}