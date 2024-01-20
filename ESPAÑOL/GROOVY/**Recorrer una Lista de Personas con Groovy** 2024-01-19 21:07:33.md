```groovy
// Clase principal de la aplicación
class MiAplicacion {

    // Método principal de la aplicación
    static void main(String[] args) {
        // Crear un objeto de tipo Persona
        Persona persona1 = new Persona("Juan", "Pérez", 30)

        // Crear un objeto de tipo Lista
        List<Persona> listaPersonas = new ArrayList<>()

        // Agregar a la lista personas el objeto persona1
        listaPersonas.add(persona1)

        // Crear un objeto de tipo Iterador
        Iterator<Persona> iteradorPersonas = listaPersonas.iterator()

        // Recorrer la lista de personas usando el iterador
        while (iteradorPersonas.hasNext()) {
            Persona persona = iteradorPersonas.next()
            println("Nombre: ${persona.nombre} ${persona.apellido}")
            println("Edad: ${persona.edad}")
        }
    }
}

// Clase que representa a una persona
class Persona {
    String nombre
    String apellido
    int edad

    // Constructor de la clase
    Persona(String nombre, String apellido, int edad) {
        this.nombre = nombre
        this.apellido = apellido
        this.edad = edad
    }
}
```

El código de arriba es un programa simple en Groovy que crea un objeto de tipo Persona, lo agrega a una lista, y luego recorre la lista usando un iterador. El código está bien documentado y es fácil de entender.

Aquí hay una explicación más detallada del código:

* La clase `MiAplicacion` es la clase principal de la aplicación. Esta clase contiene el método `main()`, que se ejecuta cuando se inicia la aplicación.
* El método `main()` crea un objeto de tipo `Persona` llamado `persona1`. El objeto `persona1` tiene los siguientes valores:
    * `nombre`: "Juan"
    * `apellido`: "Pérez"
    * `edad`: 30
* El método `main()` también crea un objeto de tipo `List` llamado `listaPersonas`. La lista `listaPersonas` es una lista de objetos de tipo `Persona`.
* El método `main()` agrega el objeto `persona1` a la lista `listaPersonas`.
* El método `main()` crea un objeto de tipo `Iterator` llamado `iteradorPersonas`. El iterador `iteradorPersonas` es un objeto que se puede usar para recorrer la lista `listaPersonas`.
* El método `main()` recorre la lista `listaPersonas` usando el iterador `iteradorPersonas`. El método `main()` usa el método `hasNext()` para verificar si el iterador tiene más elementos. Si el iterador tiene más elementos, el método `main()` llama al método `next()` para obtener el siguiente elemento y lo imprime en la consola.

El código de arriba es un ejemplo simple de cómo usar Groovy para crear objetos, listas e iteradores. Groovy es un lenguaje de programación muy versátil y potente que se puede utilizar para una amplia variedad de tareas.