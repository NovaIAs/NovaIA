```groovy
// Definición de la clase principal
class Programa {

    // Método principal
    static void main(String[] args) {

        // Definición de variables
        // Un array de números enteros
        int[] numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

        // Un mapa de clave-valor
        Map<String, String> mapa = ["nombre": "Juan", "apellido": "Pérez"]

        // Una lista de cadenas
        List<String> lista = ["rojo", "verde", "azul"]

        // Un objeto de la clase Persona
        Persona persona = new Persona("Juan", "Pérez", 25)

        // Uso de la sentencia if-else
        if (persona.edad > 18) {
            println("La persona es mayor de edad.")
        } else {
            println("La persona es menor de edad.")
        }

        // Uso de la sentencia switch-case
        switch (persona.nombre) {
            case "Juan":
                println("El nombre de la persona es Juan.")
                break
            case "María":
                println("El nombre de la persona es María.")
                break
            default:
                println("El nombre de la persona no es Juan ni María.")
        }

        // Uso de un bucle for
        for (int i = 0; i < numeros.length; i++) {
            println("El número ${numeros[i]} está en la posición ${i}.")
        }

        // Uso de un bucle while
        int contador = 0
        while (contador < 10) {
            println("El valor de contador es ${contador}.")
            contador++
        }

        // Uso de un bucle do-while
        do {
            println("El valor de contador es ${contador}.")
            contador++
        } while (contador < 10)

        // Uso de un bucle for-each
        for (String color : lista) {
            println("El color ${color} está en la lista.")
        }

        // Uso de un método de la clase Persona
        println("El nombre completo de la persona es ${persona.getNombreCompleto()}.")

        // Uso de un método estático de la clase Persona
        println("La edad mínima para ser mayor de edad es ${Persona.EDAD_MINIMA_MAYORIA_EDAD}.")

        // Uso de un constructor de la clase Persona
        Persona persona2 = new Persona("María", "López", 30)

        // Uso de un getter y un setter de la clase Persona
        persona2.setNombre("María del Carmen")
        println("El nuevo nombre de la persona es ${persona2.getNombre()}.")

        // Uso de una propiedad de la clase Persona
        println("La edad de la persona es ${persona2.edad}.")

        // Uso de un evento de la clase Persona
        persona2.addNombreChangeListener({ evento ->
            println("El nombre de la persona ha cambiado a ${evento.nuevoNombre}.")
        })
        persona2.setNombre("María del Carmen López")
    }
}

// Definición de la clase Persona
class Persona {

    // Constantes
    static final int EDAD_MINIMA_MAYORIA_EDAD = 18

    // Propiedades
    private String nombre
    private String apellido
    int edad

    // Constructor
    Persona(String nombre, String apellido, int edad) {
        this.nombre = nombre
        this.apellido = apellido
        this.edad = edad
    }

    // Métodos
    String getNombreCompleto() {
        return "${nombre} ${apellido}"
    }

    String getNombre() {
        return nombre
    }

    void setNombre(String nombre) {
        this.nombre = nombre
        fireNombreChangeEvent(nombre)
    }

    void setApellido(String apellido) {
        this.apellido = apellido
    }

    void setEdad(int edad) {
        this.edad = edad
    }

    // Evento
    def nombreChangeListeners = []

    void addNombreChangeListener(NombreChangeListener listener) {
        nombreChangeListeners << listener
    }

    void fireNombreChangeEvent(String nuevoNombre) {
        nombreChangeListeners.each { listener ->
            listener.nombreCambiado(nuevoNombre)
        }
    }
}

// Interfaz para el evento de cambio de nombre
interface NombreChangeListener {

    void nombreCambiado(String nuevoNombre)
}
```

Explicación del código:

* El código define una clase principal llamada `Programa`.
* El método `main` de la clase `Programa` es el punto de entrada del programa.
* Se definen varias variables, como un array de números enteros, un mapa de clave-valor, una lista de cadenas y un objeto de la clase `Persona`.
* Se utilizan las sentencias `if-else`, `switch-case`, `for`, `while`, `do-while` y `for-each` para controlar el flujo del programa.
* Se utilizan métodos de la clase `Persona` para obtener y modificar los atributos del objeto `persona`.
* Se utiliza un evento de la clase `Persona` para notificar a los escuchas cuando el nombre de la persona cambia.
* Se define una interfaz `NombreChangeListener` para el evento de cambio de nombre.
* Se crea una instancia de la clase `Persona` y se le añaden escuchas para el evento de cambio de nombre.
* Cuando el nombre de la persona cambia, se llama al método `nombreCambiado` de los escuchas.