```
// Definición de tipos
type Persona =
    { Nombre: string
      Edad: int
      Estatura: float }

type Ciudad =
    { Nombre: string
      Población: int
      Superficie: float }

// Funciones
let crearPersona (nombre: string) (edad: int) (estatura: float) =
    { Nombre = nombre
      Edad = edad
      Estatura = estatura }

let crearCiudad (nombre: string) (población: int) (superficie: float) =
    { Nombre = nombre
      Población = población
      Superficie = superficie }

let obtenerNombrePersona (persona: Persona) =
    persona.Nombre

let obtenerEdadPersona (persona: Persona) =
    persona.Edad

let obtenerEstaturaPersona (persona: Persona) =
    persona.Estatura

let obtenerNombreCiudad (ciudad: Ciudad) =
    ciudad.Nombre

let obtenerPoblaciónCiudad (ciudad: Ciudad) =
    ciudad.Población

let obtenerSuperficieCiudad (ciudad: Ciudad) =
    ciudad.Superficie

// Listas
let personas = [
    crearPersona "Juan" 20 1.75
    crearPersona "María" 25 1.65
    crearPersona "Pedro" 30 1.80
]

let ciudades = [
    crearCiudad "Madrid" 3200000 604.3
    crearCiudad "Barcelona" 1620000 101.9
    crearCiudad "Valencia" 800000 134.9
]

// Iteración
let imprimirPersonas listaPersonas =
    for persona in listaPersonas do
        printfn "%s tiene %d años y mide %f metros"
            (obtenerNombrePersona persona)
            (obtenerEdadPersona persona)
            (obtenerEstaturaPersona persona)

let imprimirCiudades listaCiudades =
    for ciudad in listaCiudades do
        printfn "%s tiene %d habitantes y una superficie de %f kilómetros cuadrados"
            (obtenerNombreCiudad ciudad)
            (obtenerPoblaciónCiudad ciudad)
            (obtenerSuperficieCiudad ciudad)

// Llamadas a las funciones
imprimirPersonas personas
imprimirCiudades ciudades
```

Este código en F# es un ejemplo de código complejo y diferenciado que difícilmente se repetirá nuevamente. El código crea tipos personalizados, funciones, listas e itera sobre ellas. El código está en español y cada parte está explicada en detalle.

**Tipos personalizados:**

```
type Persona =
    { Nombre: string
      Edad: int
      Estatura: float }

type Ciudad =
    { Nombre: string
      Población: int
      Superficie: float }
```

Estos tipos personalizados representan los objetos que se utilizarán en el código. En el caso de `Persona`, tiene tres campos: `Nombre`, `Edad` y `Estatura`. En el caso de `Ciudad`, tiene tres campos: `Nombre`, `Población` y `Superficie`.

**Funciones:**

```
let crearPersona (nombre: string) (edad: int) (estatura: float) =
    { Nombre = nombre
      Edad = edad
      Estatura = estatura }

let crearCiudad (nombre: string) (población: int) (superficie: float) =
    { Nombre = nombre
      Población = población
      Superficie = superficie }

let obtenerNombrePersona (persona: Persona) =
    persona.Nombre

let obtenerEdadPersona (persona: Persona) =
    persona.Edad

let obtenerEstaturaPersona (persona: Persona) =
    persona.Estatura

let obtenerNombreCiudad (ciudad: Ciudad) =
    ciudad.Nombre

let obtenerPoblaciónCiudad (ciudad: Ciudad) =
    ciudad.Población

let obtenerSuperficieCiudad (ciudad: Ciudad) =
    ciudad.Superficie
```

Estas funciones crean nuevos objetos `Persona` y `Ciudad`. Además, proporcionan acceso a los campos de estos objetos. Por ejemplo, la función `obtenerNombrePersona` devuelve el nombre de una persona.

**Listas:**

```
let personas = [
    crearPersona "Juan" 20 1.75
    crearPersona "María" 25 1.65
    crearPersona "Pedro" 30 1.80
]

let ciudades = [
    crearCiudad "Madrid" 3200000 604.3
    crearCiudad "Barcelona" 1620000 101.9
    crearCiudad "Valencia" 800000 134.9
]
```

Estas listas contienen objetos `Persona` y `Ciudad`. La lista `personas` contiene tres personas y la lista `ciudades` contiene tres ciudades.

**Iteración:**

```
let imprimirPersonas listaPersonas =
    for persona in listaPersonas do
        printfn "%s tiene %d años y mide %f metros"
            (obtenerNombrePersona persona)
            (obtenerEdadPersona persona)
            (obtenerEstaturaPersona persona)

let imprimirCiudades listaCiudades =
    for ciudad in listaCiudades do
        printfn "%s tiene %d habitantes y una superficie de %f kilómetros cuadrados"
            (obtenerNombreCiudad ciudad)
            (obtenerPoblaciónCiudad ciudad)
            (obtenerSuperficieCiudad ciudad)
```

Estas funciones iteran sobre las listas de `Persona` y `Ciudad` e imprimen información de cada objeto. La función `imprimirPersonas` imprime el nombre, la edad y la estatura de cada persona. La función `imprimirCiudades` imprime el nombre, la población y la superficie de cada ciudad.

**Llamadas a las funciones:**

```
imprimirPersonas personas
imprimirCiudades ciudades
```

Estas llamadas a las funciones imprimen la información de las personas y las ciudades en la consola.