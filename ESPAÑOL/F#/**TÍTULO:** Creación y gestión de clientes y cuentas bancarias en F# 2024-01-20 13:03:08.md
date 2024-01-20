```F#
// Definición de tipos y estructuras

type Cliente = { Nombre: string; Apellidos: string; DNI: string; }
type Cuenta = { Numero: string; Saldo: decimal; }

// Definición de módulos

module Clientes =
    // Función para crear un nuevo cliente
    let crearCliente (nombre, apellidos, dni) =
        { Nombre = nombre; Apellidos = apellidos; DNI = dni; }

    // Función para obtener el nombre completo de un cliente
    let obtenerNombreCompleto cliente =
        cliente.Nombre + " " + cliente.Apellidos

    // Función para obtener el DNI de un cliente
    let obtenerDNI cliente =
        cliente.DNI

// Definición de programa principal

let main =
    let cliente1 = Clientes.crearCliente("Juan", "García", "12345678")
    let cuenta1 = { Numero = "ES1234567890", Saldo = 1000.00m }
    printfn "%s tiene una cuenta con el número %s y un saldo de %s"
        (Clientes.obtenerNombreCompleto cliente1)
        cuenta1.Numero
        (cuenta1.Saldo.ToString("C"))

// Ejecución del programa principal

main
```

Explicación del código:

* El código está dividido en módulos. Esto ayuda a organizar el código y hacerlo más legible y mantenible.
* El módulo `Clientes` define un tipo `Cliente` y dos funciones `crearCliente` y `obtenerNombreCompleto`. La función `crearCliente` crea un nuevo cliente y la función `obtenerNombreCompleto` obtiene el nombre completo de un cliente.
* El programa principal crea un nuevo cliente y una nueva cuenta. A continuación, utiliza la función `obtenerNombreCompleto` para obtener el nombre completo del cliente y la función `ToString` para convertir el saldo de la cuenta a una cadena de texto. Finalmente, utiliza la función `printfn` para imprimir el nombre completo del cliente, el número de cuenta y el saldo de la cuenta en la consola.