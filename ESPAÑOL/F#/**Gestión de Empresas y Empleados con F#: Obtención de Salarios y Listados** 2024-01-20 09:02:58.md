```f#
// Definición de tipos
type Empresa = string[]
type Empleado = { Nombre: string; Apellido: string; Puesto: string; Salario: int }

// Datos de muestra
let empresas = [| "Acme Corporation"; "XYZ Enterprises"; "MegaTech Industries" |]
let empleados = [|
    { Nombre = "Juan"; Apellido = "Pérez"; Puesto = "Gerente"; Salario = 100000 };
    { Nombre = "María"; Apellido = "García"; Puesto = "Contadora"; Salario = 75000 };
    { Nombre = "Pedro"; Apellido = "López"; Puesto = "Ingeniero"; Salario = 50000 };
|]

// Función para obtener los empleados de una empresa
let getEmpleadosEmpresa empresa =
    let empleadosEmpresa = empleados |> List.filter (fun empleado -> Array.exists (fun e -> e = empleado.Nombre) empresa)
    empleadosEmpresa

// Función para obtener el salario total de una empresa
let getSalarioTotalEmpresa empresa =
    getEmpleadosEmpresa empresa |> List.sumBy (fun empleado -> empleado.Salario)

// Función para obtener la empresa con el mayor salario total
let getEmpresaMayorSalarioTotal () =
    empresas |> Array.maxBy (fun empresa -> getSalarioTotalEmpresa empresa)

// Función para obtener los empleados mejor pagados de una empresa
let getEmpleadosMejorPagadosEmpresa empresa n =
    getEmpleadosEmpresa empresa |> List.sortBy (fun empleado -> empleado.Salario) |> List.rev |> List.take n

// Función para obtener los empleados mejor pagados de todas las empresas
let getEmpleadosMejorPagadosTodasEmpresas n =
    empresas |> Array.map getEmpleadosMejorPagadosEmpresa n |> List.concat |> List.sortBy (fun empleado -> empleado.Salario) |> List.rev |> List.take n

// Imprimir resultados
printfn $"Empresas: {String.join ", " empresas}"
printfn $"Empleados: {String.join ", " empleados}"
printfn $"Empleados de Acme Corporation: {String.join ", " (getEmpleadosEmpresa "Acme Corporation")}"
printfn $"Salario total de Acme Corporation: {getSalarioTotalEmpresa "Acme Corporation"}"
printfn $"Empresa con mayor salario total: {getEmpresaMayorSalarioTotal ()}"
printfn $"Empleados mejor pagados de XYZ Enterprises (3): {String.join ", " (getEmpleadosMejorPagadosEmpresa "XYZ Enterprises" 3)}"
printfn $"Empleados mejor pagados de todas las empresas (5): {String.join ", " (getEmpleadosMejorPagadosTodasEmpresas 5)}"
```

Explicación del código:

1. Definimos los tipos `Empresa` y `Empleado` utilizando la característica "Discriminated Union" de F#, lo que nos permite combinar diferentes tipos en una sola estructura.

2. Creamos algunos datos de muestra, incluyendo tres empresas y tres empleados con sus respectivos nombres, apellidos, puestos y salarios.

3. Definimos varias funciones para trabajar con los datos:
   - `getEmpleadosEmpresa` obtiene los empleados de una empresa determinada.
   - `getSalarioTotalEmpresa` obtiene el salario total de una empresa determinada.
   - `getEmpresaMayorSalarioTotal` obtiene la empresa con el mayor salario total.
   - `getEmpleadosMejorPagadosEmpresa` obtiene los n empleados mejor pagados de una empresa determinada.
   - `getEmpleadosMejorPagadosTodasEmpresas` obtiene los n empleados mejor pagados de todas las empresas.

4. Imprimimos los resultados de las funciones en la consola utilizando la función `printfn`.

Este código demuestra varios conceptos importantes de F#, incluyendo:

* Tipos discriminados
* Funciones de orden superior
* Listas y listas compuestas
* Secuencias de comprensión
* Funciones de filtro, mapa y reducción
* Funciones de ordenación y toma