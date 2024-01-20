```groovy
// Definimos una clase llamada "Persona" con sus atributos y métodos
class Persona {
    String nombre
    int edad
    String ciudad

    // Método para mostrar los datos de la persona
    def mostrarDatos() {
        println "Nombre: $nombre"
        println "Edad: $edad"
        println "Ciudad: $ciudad"
    }
}

// Creamos una lista de personas
def listaPersonas = []

// Agregamos algunas personas a la lista
listaPersonas.add(new Persona(nombre: "Juan", edad: 20, ciudad: "Madrid"))
listaPersonas.add(new Persona(nombre: "María", edad: 25, ciudad: "Barcelona"))
listaPersonas.add(new Persona(nombre: "Pedro", edad: 30, ciudad: "Sevilla"))

// Recorremos la lista de personas utilizando una expresión lambda
listaPersonas.each { persona -> persona.mostrarDatos() }

// Creamos un mapa para almacenar los datos de las personas por su nombre
def mapaPersonas = [:]

// Agregamos las personas al mapa utilizando el nombre como clave
listaPersonas.each { persona -> mapaPersonas[persona.nombre] = persona }

// Mostramos los datos de las personas en el mapa
mapaPersonas.each { nombre, persona -> persona.mostrarDatos() }

// Filtramos la lista de personas por las que sean mayores de 25 años
def listaPersonasMayores25 = listaPersonas.findAll { persona -> persona.edad > 25 }

// Mostramos los datos de las personas mayores de 25 años
listaPersonasMayores25.each { persona -> persona.mostrarDatos() }

// Ordenamos la lista de personas por edad en orden ascendente
def listaPersonasOrdenadas = listaPersonas.sort { persona1, persona2 -> persona1.edad <=> persona2.edad }

// Mostramos los datos de las personas ordenadas por edad
listaPersonasOrdenadas.each { persona -> persona.mostrarDatos() }

// Definimos una función para calcular la edad media de las personas
def calcularEdadMedia() {
    def edadTotal = 0
    listaPersonas.each { persona -> edadTotal += persona.edad }
    edadTotal / listaPersonas.size()
}

// Mostramos la edad media de las personas
println "Edad media: ${calcularEdadMedia()}"
```

Explicación del código:

* Definimos una clase llamada "Persona" con sus atributos y métodos.
* Creamos una lista de personas y agregamos algunas personas a la lista.
* Recorremos la lista de personas utilizando una expresión lambda y mostramos los datos de cada persona.
* Creamos un mapa para almacenar los datos de las personas por su nombre y agregamos las personas al mapa utilizando el nombre como clave.
* Mostramos los datos de las personas en el mapa.
* Filtramos la lista de personas por las que sean mayores de 25 años y mostramos los datos de las personas mayores de 25 años.
* Ordenamos la lista de personas por edad en orden ascendente y mostramos los datos de las personas ordenadas por edad.
* Definimos una función para calcular la edad media de las personas y mostramos la edad media de las personas.