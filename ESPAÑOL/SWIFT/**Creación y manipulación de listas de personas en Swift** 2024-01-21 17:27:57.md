```swift
// Definición de una estructura para representar una persona
struct Persona {
    var nombre: String
    var apellido: String
    var edad: Int
    var ocupacion: String
}

// Definición de una función para crear una lista de personas
func crearListaPersonas() -> [Persona] {
    var listaPersonas: [Persona] = []
    
    // Añadimos algunas personas a la lista
    listaPersonas.append(Persona(nombre: "Juan", apellido: "García", edad: 25, ocupacion: "Ingeniero"))
    listaPersonas.append(Persona(nombre: "María", apellido: "Pérez", edad: 30, ocupacion: "Médica"))
    listaPersonas.append(Persona(nombre: "Pedro", apellido: "López", edad: 40, ocupacion: "Profesor"))
    
    return listaPersonas
}

// Definición de una función para imprimir la información de una lista de personas
func imprimirListaPersonas(listaPersonas: [Persona]) {
    for persona in listaPersonas {
        print("Nombre: \(persona.nombre) \(persona.apellido)")
        print("Edad: \(persona.edad)")
        print("Ocupación: \(persona.ocupacion)")
        print("----------------------")
    }
}

// Definición de una función para filtrar una lista de personas por edad
func filtrarListaPersonasPorEdad(listaPersonas: [Persona], edadMinima: Int, edadMaxima: Int) -> [Persona] {
    var listaPersonasFiltrada: [Persona] = []
    
    for persona in listaPersonas {
        if persona.edad >= edadMinima && persona.edad <= edadMaxima {
            listaPersonasFiltrada.append(persona)
        }
    }
    
    return listaPersonasFiltrada
}

// Definición de una función para ordenar una lista de personas por edad
func ordenarListaPersonasPorEdad(listaPersonas: [Persona]) -> [Persona] {
    var listaPersonasOrdenada: [Persona] = listaPersonas
    
    listaPersonasOrdenada.sort { $0.edad < $1.edad }
    
    return listaPersonasOrdenada
}

// Definición de una función para agrupar una lista de personas por ocupación
func agruparListaPersonasPorOcupacion(listaPersonas: [Persona]) -> [String: [Persona]] {
    var gruposPersonas: [String: [Persona]] = [:]
    
    for persona in listaPersonas {
        if gruposPersonas[persona.ocupacion] == nil {
            gruposPersonas[persona.ocupacion] = []
        }
        
        gruposPersonas[persona.ocupacion]?.append(persona)
    }
    
    return gruposPersonas
}

// Creación de una lista de personas
var listaPersonas: [Persona] = crearListaPersonas()

// Impresión de la información de la lista de personas
print("Lista de personas:")
imprimirListaPersonas(listaPersonas: listaPersonas)

// Filtrado de la lista de personas por edad
var listaPersonasFiltrada: [Persona] = filtrarListaPersonasPorEdad(listaPersonas: listaPersonas, edadMinima: 20, edadMaxima: 30)

// Impresión de la información de la lista de personas filtrada
print("Lista de personas filtrada por edad:")
imprimirListaPersonas(listaPersonas: listaPersonasFiltrada)

// Ordenación de la lista de personas por edad
var listaPersonasOrdenada: [Persona] = ordenarListaPersonasPorEdad(listaPersonas: listaPersonas)

// Impresión de la información de la lista de personas ordenada
print("Lista de personas ordenada por edad:")
imprimirListaPersonas(listaPersonas: listaPersonasOrdenada)

// Agrupación de la lista de personas por ocupación
var gruposPersonas: [String: [Persona]] = agruparListaPersonasPorOcupacion(listaPersonas: listaPersonas)

// Impresión de la información de los grupos de personas
print("Grupos de personas por ocupación:")
for (ocupacion, personas) in gruposPersonas {
    print("Ocupación: \(ocupacion)")
    imprimirListaPersonas(listaPersonas: personas)
    print("----------------------")
}
```

**Explicación del código:**

1. Definimos una estructura `Persona` que representa una persona con los atributos nombre, apellido, edad y ocupación.
2. Definimos una función `crearListaPersonas()` que crea una lista de personas y la devuelve.
3. Definimos una función `imprimirListaPersonas()` que imprime la información de una lista de personas.
4. Definimos una función `filtrarListaPersonasPorEdad()` que filtra una lista de personas por edad y devuelve la lista de personas filtrada.
5. Definimos una función `ordenarListaPersonasPorEdad()` que ordena una lista de personas por edad y devuelve la lista de personas ordenada.
6. Definimos una función `agruparListaPersonasPorOcupacion()` que agrupa una lista de personas por ocupación y devuelve un diccionario donde las claves son las ocupaciones y los valores son las listas de personas que tienen esa ocupación.
7. Creamos una lista de personas utilizando la función `crearListaPersonas()`.
8. Imprimimos la información de la lista de personas utilizando la función `imprimirListaPersonas()`.
9. Filtramos la lista de personas por edad utilizando la función `filtrarListaPersonasPorEdad()`.
10. Imprimimos la información de la lista de personas filtrada utilizando la función `imprimirListaPersonas()`.
11. Ordenamos la lista de personas por edad utilizando la función `ordenarListaPersonasPorEdad()`.
12. Imprimimos la información de la lista de personas ordenada utilizando la función `imprimirListaPersonas()`.
13. Agrupamos la lista de personas por ocupación utilizando la función `agruparListaPersonasPorOcupacion()`.
14. Imprimimos la información de los grupos de personas utilizando un bucle `for`.