```f#
// Módulo principal
module Programa =

    // Definición de tipos
    type Persona = { Nombre: string; Apellido: string; Edad: int }

    // Definición de funciones
    let saludar persona =
        printfn "Hola, %s %s! Tienes %d años." persona.Nombre persona.Apellido persona.Edad

    // Función principal
    let main args =
        // Crear una lista de personas
        let personas = [
            { Nombre = "Juan"; Apellido = "García"; Edad = 25 };
            { Nombre = "María"; Apellido = "López"; Edad = 30 };
            { Nombre = "Pedro"; Apellido = "Sánchez"; Edad = 35 }
        ]

        // Iterar sobre la lista de personas y saludar a cada una
        for persona in personas do
            saludar persona

        // Devolver 0 para indicar que el programa se ejecutó correctamente
        0

// Punto de entrada del programa
[<EntryPoint>]
let main _ =
    Programa.main []
```

Explicación:

1. El código define un tipo `Persona` que representa a una persona con los campos `Nombre`, `Apellido` y `Edad`.

2. También define una función `saludar` que toma una persona como argumento y muestra un mensaje de saludo personalizado.

3. La función `main` crea una lista de personas y luego usa un bucle `for` para iterar sobre la lista y saludar a cada persona.

4. Finalmente, el punto de entrada del programa llama a la función `main` con una lista vacía de argumentos, lo que inicia la ejecución del programa.