```kotlin
// Definir una clase de datos para representar a una persona
data class Persona(val nombre: String, val apellido: String, val edad: Int)

// Definir una clase de datos para representar a una dirección
data class Direccion(val calle: String, val numero: Int, val ciudad: String, val provincia: String, val codigoPostal: Int)

// Definir una clase de datos para representar a un contacto
data class Contacto(val persona: Persona, val direccion: Direccion, val telefono: String, val email: String)

// Definir una función para crear una lista de contactos
fun crearListaContactos(): List<Contacto> {
    val listaContactos = mutableListOf<Contacto>()

    // Agregar algunos contactos a la lista
    listaContactos.add(Contacto(Persona("Juan", "Pérez", 25), Direccion("Calle Mayor", 123, "Madrid", "Madrid", 28013), "123456789", "juan.perez@email.com"))
    listaContactos.add(Contacto(Persona("María", "López", 30), Direccion("Calle Menor", 456, "Barcelona", "Barcelona", 08001), "987654321", "maria.lopez@email.com"))
    listaContactos.add(Contacto(Persona("Pedro", "García", 35), Direccion("Calle Real", 789, "Valencia", "Valencia", 46001), "0123456789", "pedro.garcia@email.com"))

    return listaContactos
}

// Definir una función para imprimir la lista de contactos
fun imprimirListaContactos(listaContactos: List<Contacto>) {
    for (contacto in listaContactos) {
        println("Nombre: ${contacto.persona.nombre} ${contacto.persona.apellido}")
        println("Dirección: ${contacto.direccion.calle} ${contacto.direccion.numero}, ${contacto.direccion.ciudad}, ${contacto.direccion.provincia}, ${contacto.direccion.codigoPostal}")
        println("Teléfono: ${contacto.telefono}")
        println("Email: ${contacto.email}")
        println()
    }
}

// Definir una función para buscar un contacto por su nombre
fun buscarContactoPorNombre(listaContactos: List<Contacto>, nombre: String): Contacto? {
    for (contacto in listaContactos) {
        if (contacto.persona.nombre == nombre) {
            return contacto
        }
    }

    return null
}

// Definir una función para eliminar un contacto por su nombre
fun eliminarContactoPorNombre(listaContactos: List<Contacto>, nombre: String) {
    listaContactos.removeIf { contacto -> contacto.persona.nombre == nombre }
}

// Crear una lista de contactos
val listaContactos = crearListaContactos()

// Imprimir la lista de contactos
imprimirListaContactos(listaContactos)

// Buscar un contacto por su nombre
val contacto = buscarContactoPorNombre(listaContactos, "María")

// Imprimir el contacto encontrado
if (contacto != null) {
    println("Contacto encontrado:")
    println("Nombre: ${contacto.persona.nombre} ${contacto.persona.apellido}")
    println("Dirección: ${contacto.direccion.calle} ${contacto.direccion.numero}, ${contacto.direccion.ciudad}, ${contacto.direccion.provincia}, ${contacto.direccion.codigoPostal}")
    println("Teléfono: ${contacto.telefono}")
    println("Email: ${contacto.email}")
} else {
    println("No se encontró ningún contacto con ese nombre.")
}

// Eliminar un contacto por su nombre
eliminarContactoPorNombre(listaContactos, "Pedro")

// Imprimir la lista de contactos después de eliminar el contacto
imprimirListaContactos(listaContactos)
```

Explicación del código:

* La clase `Persona` representa a una persona con su nombre, apellido y edad.
* La clase `Direccion` representa una dirección con su calle, número, ciudad, provincia y código postal.
* La clase `Contacto` representa a un contacto con su persona, dirección, teléfono y email.
* La función `crearListaContactos()` crea una lista de contactos.
* La función `imprimirListaContactos()` imprime la lista de contactos.
* La función `buscarContactoPorNombre()` busca un contacto por su nombre.
* La función `eliminarContactoPorNombre()` elimina un contacto por su nombre.

En el código principal, primero se crea una lista de contactos. Luego, se imprime la lista de contactos. A continuación, se busca un contacto por su nombre y se imprime el contacto encontrado. Por último, se elimina un contacto por su nombre y se imprime la lista de contactos después de eliminar el contacto.