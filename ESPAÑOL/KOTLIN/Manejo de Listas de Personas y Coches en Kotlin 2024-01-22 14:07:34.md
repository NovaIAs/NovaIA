```kotlin
// Definimos una clase llamada "Persona" con propiedades "nombre" y "edad".
class Persona(val nombre: String, val edad: Int) {
    // Función que devuelve el nombre de la persona en mayúsculas.
    fun obtenerNombreEnMayusculas() = nombre.toUpperCase()

    // Función que devuelve la edad de la persona en años.
    fun obtenerEdadEnAños() = "$edad años"
}

// Definimos una clase llamada "Coche" con propiedades "marca", "modelo" y "año".
class Coche(val marca: String, val modelo: String, val año: Int) {
    // Función que devuelve la marca y el modelo del coche.
    fun obtenerMarcaYModelo() = "$marca $modelo"

    // Función que devuelve el año del coche.
    fun obtenerAño() = año
}

// Definimos una clase llamada "ListaDePersonas" que hereda de la clase "ArrayList".
class ListaDePersonas : ArrayList<Persona>() {
    // Función que devuelve el número de personas en la lista.
    fun obtenerNumeroDePersonas() = size

    // Función que devuelve la persona con el nombre especificado.
    fun obtenerPersonaPorNombre(nombre: String): Persona? {
        return find { persona -> persona.nombre == nombre }
    }
}

// Definimos una clase llamada "ListaDeCoches" que hereda de la clase "ArrayList".
class ListaDeCoches : ArrayList<Coche>() {
    // Función que devuelve el número de coches en la lista.
    fun obtenerNumeroDeCoches() = size

    // Función que devuelve el coche con la marca y el modelo especificados.
    fun obtenerCochePorMarcaYModelo(marca: String, modelo: String): Coche? {
        return find { coche -> coche.marca == marca && coche.modelo == modelo }
    }
}

// Creamos una lista de personas.
val listaDePersonas = ListaDePersonas()

// Añadimos algunas personas a la lista.
listaDePersonas.add(Persona("Juan", 25))
listaDePersonas.add(Persona("María", 30))
listaDePersonas.add(Persona("Pedro", 35))

// Creamos una lista de coches.
val listaDeCoches = ListaDeCoches()

// Añadimos algunos coches a la lista.
listaDeCoches.add(Coche("Toyota", "Corolla", 2015))
listaDeCoches.add(Coche("Honda", "Civic", 2016))
listaDeCoches.add(Coche("Ford", "Focus", 2017))

// Imprimimos el número de personas en la lista.
println("Número de personas en la lista: ${listaDePersonas.obtenerNumeroDePersonas()}")

// Imprimimos el nombre de la primera persona en la lista.
println("Nombre de la primera persona en la lista: ${listaDePersonas[0].nombre}")

// Imprimimos el nombre de la persona con el nombre "María".
println("Nombre de la persona con el nombre \"María\": ${listaDePersonas.obtenerPersonaPorNombre("María")?.nombre}")

// Imprimimos el número de coches en la lista.
println("Número de coches en la lista: ${listaDeCoches.obtenerNumeroDeCoches()}")

// Imprimimos la marca y el modelo del primer coche en la lista.
println("Marca y modelo del primer coche en la lista: ${listaDeCoches[0].obtenerMarcaYModelo()}")

// Imprimimos la marca y el modelo del coche con la marca "Ford" y el modelo "Focus".
println("Marca y modelo del coche con la marca \"Ford\" y el modelo \"Focus\": ${listaDeCoches.obtenerCochePorMarcaYModelo("Ford", "Focus")?.obtenerMarcaYModelo()}")
```

Explicación del código:

* Definimos las clases `Persona` y `Coche` con sus propiedades y funciones correspondientes.
* Definimos las clases `ListaDePersonas` y `ListaDeCoches` que heredan de la clase `ArrayList` y añaden algunas funciones adicionales.
* Creamos una lista de personas y una lista de coches.
* Añadimos algunas personas y coches a las listas.
* Imprimimos el número de personas y coches en las listas.
* Imprimimos el nombre de la primera persona y del primer coche en las listas.
* Imprimimos el nombre de la persona con el nombre "María" y la marca y el modelo del coche con la marca "Ford" y el modelo "Focus".