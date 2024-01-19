```f#
// Definición de tipos

type Persona =
    { Nombre: string
      Edad: int }

type Direccion =
    { Calle: string
      Numero: int
      Ciudad: string }

// Definición de funciones

// Función para crear una persona con un nombre y una edad dados.
let crearPersona (nombre: string) (edad: int) =
    { Nombre = nombre
      Edad = edad }

// Función para crear una dirección con una calle, un número y una ciudad dados.
let crearDireccion (calle: string) (numero: int) (ciudad: string) =
    { Calle = calle
      Numero = numero
      Ciudad = ciudad }

// Función para imprimir una persona en la consola.
let imprimirPersona (persona: Persona) =
    printfn "%s (%d años)" persona.Nombre persona.Edad

// Función para imprimir una dirección en la consola.
let imprimirDireccion (direccion: Direccion) =
    printfn "%s %d, %s" direccion.Calle direccion.Numero direccion.Ciudad

// Función para crear una lista de personas.
let crearListaPersonas () =
    [ crearPersona "Juan" 20
      crearPersona "María" 25
      crearPersona "Pedro" 30 ]

// Función para crear una lista de direcciones.
let crearListaDirecciones () =
    [ crearDireccion "Calle Mayor" 123 "Madrid"
      crearDireccion "Calle Menor" 456 "Barcelona"
      crearDireccion "Calle Ancha" 789 "Valencia" ]

// Función principal del programa.
let main () =
    let personas = crearListaPersonas ()
    let direcciones = crearListaDirecciones ()

    // Iterar sobre la lista de personas e imprimirlas en la consola.
    List.iter imprimirPersona personas

    // Iterar sobre la lista de direcciones e imprimirlas en la consola.
    List.iter imprimirDireccion direcciones
```

Explicación del código:

* **Definición de tipos:** Se definen los tipos `Persona` y `Direccion` utilizando el tipo de registro `type`. El tipo `Persona` tiene dos campos: `Nombre` (de tipo `string`) y `Edad` (de tipo `int`). El tipo `Direccion` tiene tres campos: `Calle` (de tipo `string`), `Numero` (de tipo `int`) y `Ciudad` (de tipo `string`).


* **Definición de funciones:** Se definen varias funciones para crear personas, direcciones, imprimir personas, imprimir direcciones, y crear listas de personas y direcciones.


* **Función principal del programa:** La función `main` es la función principal del programa. Crea una lista de personas y una lista de direcciones utilizando las funciones definidas anteriormente, y luego itera sobre ellas e imprime cada persona y dirección en la consola.