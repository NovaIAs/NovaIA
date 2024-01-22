```go
// Definimos un tipo de dato personalizado llamado "Persona" con los campos "nombre", "edad" y "ocupación".
type Persona struct {
    nombre string
    edad int
    ocupación string
}

// Esta función crea una nueva instancia del tipo de dato "Persona".
func NuevaPersona(nombre string, edad int, ocupación string) Persona {
    return Persona{nombre, edad, ocupación}
}

// Esta función imprime los datos de una persona en la consola.
func ImprimirPersona(persona Persona) {
    fmt.Printf("Nombre: %s\n", persona.nombre)
    fmt.Printf("Edad: %d\n", persona.edad)
    fmt.Printf("Ocupación: %s\n", persona.ocupación)
}

// Esta función crea una lista de personas.
func CrearListaPersonas() []Persona {
    personas := []Persona{
        NuevaPersona("Juan", 25, "Ingeniero de Software"),
        NuevaPersona("María", 30, "Contadora"),
        NuevaPersona("Pedro", 35, "Médico"),
    }
    return personas
}

// Esta función imprime los datos de una lista de personas en la consola.
func ImprimirListaPersonas(personas []Persona) {
    for _, persona := range personas {
        ImprimirPersona(persona)
        fmt.Println()
    }
}

// Función principal del programa.
func main() {
    // Creamos una lista de personas.
    personas := CrearListaPersonas()

    // Imprimimos los datos de la lista de personas en la consola.
    fmt.Println("Lista de personas:")
    ImprimirListaPersonas(personas)
}
```

Explicación:

* Definimos un tipo de dato personalizado llamado "Persona" con los campos "nombre", "edad" y "ocupación".
* Creamos una función llamada "NuevaPersona" que crea una nueva instancia del tipo de dato "Persona".
* Creamos una función llamada "ImprimirPersona" que imprime los datos de una persona en la consola.
* Creamos una función llamada "CrearListaPersonas" que crea una lista de personas.
* Creamos una función llamada "ImprimirListaPersonas" que imprime los datos de una lista de personas en la consola.
* En la función principal del programa, creamos una lista de personas y luego imprimimos los datos de la lista de personas en la consola.