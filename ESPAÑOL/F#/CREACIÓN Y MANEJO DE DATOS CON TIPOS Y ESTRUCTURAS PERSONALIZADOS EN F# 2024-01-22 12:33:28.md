```f#

// Creación de tipos y estructuras

// Definiendo un tipo "Persona" con tres campos: nombre, apellido y edad
type Persona = { Nombre : string; Apellido : string; Edad : int }

// Definiendo una estructura "Dirección" con cuatro campos: calle, ciudad, estado y código postal
struct Dirección =
  { Calle : string; Ciudad : string; Estado : string; CódigoPostal : int }

// Definiendo un tipo "CuentaBancaria" con cinco campos: número de cuenta, saldo, tipo de cuenta, fecha de apertura y estado
type CuentaBancaria =
  { NúmeroCuenta : int; Saldo : float; TipoCuenta : string; FechaApertura : DateTime; Estado : bool }

// Creación de funciones

// Función para crear una persona con un nombre, apellido y edad dados
let crearPersona (nombre, apellido, edad) =
  { Nombre = nombre; Apellido = apellido; Edad = edad }

// Función para crear una dirección con una calle, ciudad, estado y código postal dados
let crearDirección (calle, ciudad, estado, códigoPostal) =
  { Calle = calle; Ciudad = ciudad; Estado = estado; CódigoPostal = códigoPostal }

// Función para crear una cuenta bancaria con un número de cuenta, saldo, tipo de cuenta, fecha de apertura y estado dados
let crearCuentaBancaria (númeroCuenta, saldo, tipoCuenta, fechaApertura, estado) =
  { NúmeroCuenta = númeroCuenta; Saldo = saldo; TipoCuenta = tipoCuenta; FechaApertura = fechaApertura; Estado = estado }

// Función para imprimir los datos de una persona
let imprimirPersona (persona : Persona) =
  printfn "%s %s, edad: %d" persona.Nombre persona.Apellido persona.Edad

// Función para imprimir los datos de una dirección
let imprimirDirección (dirección : Dirección) =
  printfn "%s, %s, %s %d" dirección.Calle dirección.Ciudad dirección.Estado dirección.CódigoPostal

// Función para imprimir los datos de una cuenta bancaria
let imprimirCuentaBancaria (cuenta : CuentaBancaria) =
  printfn "Número de cuenta: %d, Saldo: %f, Tipo de cuenta: %s, Fecha de apertura: %s, Estado: %b" cuenta.NúmeroCuenta cuenta.Saldo cuenta.TipoCuenta cuenta.FechaApertura cuenta.Estado

// Creación de instancias

// Creando una persona llamada "Juan Pérez" con 25 años de edad
let juanPerez = crearPersona ("Juan", "Pérez", 25)

// Creando una dirección con la calle "Calle Mayor", ciudad "Madrid", estado "España" y código postal 28013
let direccionJuanPerez = crearDirección ("Calle Mayor", "Madrid", "España", 28013)

// Creando una cuenta bancaria con el número 123456789, saldo 1000 euros, tipo de cuenta "Corriente", fecha de apertura 12/01/2023 y estado "Activa"
let cuentaJuanPerez = crearCuentaBancaria (123456789, 1000.0, "Corriente", DateTime.Parse("12/01/2023"), true)

// Imprimiendo los datos de la persona, la dirección y la cuenta bancaria
imprimirPersona juanPerez
imprimirDirección direccionJuanPerez
imprimirCuentaBancaria cuentaJuanPerez

```

Explicación del código:

1. Se definen los tipos "Persona", "Dirección" y "CuentaBancaria" con sus respectivos campos.
2. Se definen las funciones "crearPersona", "crearDirección" y "crearCuentaBancaria" para crear instancias de estos tipos.
3. Se definen las funciones "imprimirPersona", "imprimirDirección" y "imprimirCuentaBancaria" para imprimir los datos de cada tipo.
4. Se crean instancias de los tipos "Persona", "Dirección" y "CuentaBancaria" con valores ficticios.
5. Se imprimen los datos de la persona, la dirección y la cuenta bancaria.