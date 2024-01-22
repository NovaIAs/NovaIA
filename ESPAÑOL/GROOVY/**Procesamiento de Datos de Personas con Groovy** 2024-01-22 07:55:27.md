```groovy
// Definir una clase de dominio
class Persona {
    String nombre
    String apellido
    Integer edad
}

// Crear una instancia de la clase de dominio
def persona = new Persona(nombre: "Juan", apellido: "García", edad: 25)

// Crear una lista de personas
def personas = [
    new Persona(nombre: "María", apellido: "López", edad: 30),
    new Persona(nombre: "Pedro", apellido: "Sánchez", edad: 35),
    new Persona(nombre: "Ana", apellido: "Fernández", edad: 40)
]

// Imprimir la lista de personas
println personas

// Obtener la lista de nombres de las personas
def nombres = personas.collect { it.nombre }

// Imprimir la lista de nombres
println nombres

// Obtener la suma de las edades de las personas
def sumaEdades = personas.sum { it.edad }

// Imprimir la suma de las edades
println sumaEdades

// Obtener la media de las edades de las personas
def mediaEdades = personas.average { it.edad }

// Imprimir la media de las edades
println mediaEdades

// Obtener la lista de personas que tienen más de 30 años
def personasMayores30 = personas.findAll { it.edad > 30 }

// Imprimir la lista de personas que tienen más de 30 años
println personasMayores30

// Obtener la lista de personas que se llaman Juan
def personasJuan = personas.findAll { it.nombre == "Juan" }

// Imprimir la lista de personas que se llaman Juan
println personasJuan

// Obtener la primera persona de la lista
def primeraPersona = personas.first()

// Imprimir la primera persona de la lista
println primeraPersona

// Obtener la última persona de la lista
def ultimaPersona = personas.last()

// Imprimir la última persona de la lista
println ultimaPersona

// Obtener la persona más joven de la lista
def personaMasJoven = personas.min { it.edad }

// Imprimir la persona más joven de la lista
println personaMasJoven

// Obtener la persona más mayor de la lista
def personaMasMayor = personas.max { it.edad }

// Imprimir la persona más mayor de la lista
println personaMasMayor

// Ordenar la lista de personas por edad
def personasOrdenadas = personas.sort { it.edad }

// Imprimir la lista de personas ordenadas por edad
println personasOrdenadas

// Ordenar la lista de personas por nombre
def personasOrdenadasPorNombre = personas.sort { it.nombre }

// Imprimir la lista de personas ordenadas por nombre
println personasOrdenadasPorNombre

// Dividir la lista de personas en dos listas, una con los nombres y otra con los apellidos
def (nombres, apellidos) = personas*.{ it.nombre, it.apellido }

// Imprimir la lista de nombres y apellidos
println nombres
println apellidos

// Crear un mapa con los nombres de las personas como claves y sus apellidos como valores
def mapaPersonas = personas.collectEntries { [it.nombre, it.apellido] }

// Imprimir el mapa de personas
println mapaPersonas

// Crear una lista de tuplas con los nombres y apellidos de las personas
def listaTuplas = personas.collect { [it.nombre, it.apellido] }

// Imprimir la lista de tuplas
println listaTuplas

// Crear una lista de objetos anónimos con los nombres y apellidos de las personas
def listaObjetosAnonimos = personas.collect { new(nombre: it.nombre, apellido: it.apellido) }

// Imprimir la lista de objetos anónimos
println listaObjetosAnonimos

```

Explicación del código:

* Se define una clase de dominio llamada `Persona` con tres propiedades: `nombre`, `apellido` y `edad`.
* Se crea una instancia de la clase `Persona` y se guarda en la variable `persona`.
* Se crea una lista de personas y se guarda en la variable `personas`.
* Se imprime la lista de personas.
* Se obtiene la lista de nombres de las personas y se guarda en la variable `nombres`.
* Se imprime la lista de nombres.
* Se obtiene la suma de las edades de las personas y se guarda en la variable `sumaEdades`.
* Se imprime la suma de las edades.
* Se obtiene la media de las edades de las personas y se guarda en la variable `mediaEdades`.
* Se imprime la media de las edades.
* Se obtiene la lista de personas que tienen más de 30 años y se guarda en la variable `personasMayores30`.
* Se imprime la lista de personas que tienen más de 30 años.
* Se obtiene la lista de personas que se llaman Juan y se guarda en la variable `personasJuan`.
* Se imprime la lista de personas que se llaman Juan.
* Se obtiene la primera persona de la lista y se guarda en la variable `primeraPersona`.
* Se imprime la primera persona de la lista.
* Se obtiene la última persona de la lista y se guarda en la variable `ultimaPersona`.
* Se imprime la última persona de la lista.
* Se obtiene la persona más joven de la lista y se guarda en la variable `personaMasJoven`.
* Se imprime la persona más joven de la lista.
* Se obtiene la persona más mayor de la lista y se guarda en la variable `personaMasMayor`.
* Se imprime la persona más mayor de la lista.
* Se ordena la lista de personas por edad y se guarda en la variable `personasOrdenadas`.
* Se imprime la lista de personas ordenadas por edad.
* Se ordena la lista de personas por nombre y se guarda en la variable `personasOrdenadasPorNombre`.
* Se imprime la lista de personas ordenadas por nombre.
* Se divide la lista de personas en dos listas, una con los nombres y otra con los apellidos, y se guardan en las variables `nombres` y `apellidos` respectivamente.
* Se imprimen las listas de nombres y apellidos.
* Se crea un mapa con los nombres de las personas como claves y sus apellidos como valores, y se guarda en la variable `mapaPersonas`.
* Se imprime el mapa de personas.
* Se crea una lista de tuplas con los nombres y apellidos de las personas, y se guarda en la variable `listaTuplas`.
* Se imprime la lista de tuplas.
* Se crea una lista de objetos anónimos con los nombres y apellidos de las personas, y se guarda en la variable `listaObjetosAnonimos`.
* Se imprime la lista de objetos anónimos.