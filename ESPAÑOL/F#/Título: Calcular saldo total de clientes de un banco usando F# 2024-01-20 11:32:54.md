```f#

// Definimos una clase "Cuenta" que representa una cuenta bancaria.
type Cuenta =
    { Numero: string
      Saldo: decimal }

// Definimos una clase "Cliente" que representa a un cliente del banco.
type Cliente =
    { Nombre: string
      Cuentas: Cuenta list }

// Creamos una lista de clientes.
let clientes =
    [ Cliente { Nombre = "Juan"; Cuentas = [ Cuenta { Numero = "123456"; Saldo = 1000.0m } ] }
      Cliente { Nombre = "María"; Cuentas = [ Cuenta { Numero = "654321"; Saldo = 2000.0m } ] } ]

// Definimos una función para obtener el saldo total de un cliente.
let saldoTotal cliente =
    cliente.Cuentas |> List.sumBy (fun cuenta -> cuenta.Saldo)

// Imprimimos el nombre de los clientes y sus saldos totales.
clientes |> List.iter (fun cliente -> printf "%s: %m\n" cliente.Nombre (saldoTotal cliente))

```

Explicación del código:

* Definimos una clase `Cuenta` que representa una cuenta bancaria. La clase tiene dos propiedades: `Numero` y `Saldo`.
* Definimos una clase `Cliente` que representa a un cliente del banco. La clase tiene dos propiedades: `Nombre` y `Cuentas`.
* Creamos una lista de clientes.
* Definimos una función `saldoTotal` para obtener el saldo total de un cliente. La función suma los saldos de todas las cuentas del cliente.
* Imprimimos el nombre de los clientes y sus saldos totales.