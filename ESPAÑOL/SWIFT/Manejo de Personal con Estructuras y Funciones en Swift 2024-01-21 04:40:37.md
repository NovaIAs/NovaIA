```swift
import Foundation

// Definimos una estructura con campos que representan los datos de un empleado.
struct Empleado {
    let nombre: String
    let apellido: String
    let edad: Int
    let salario: Double
    let departamento: String
}

// Creamos una lista de empleados con algunos datos de ejemplo.
let empleados: [Empleado] = [
    Empleado(nombre: "Juan", apellido: " Pérez", edad: 25, salario: 1500.0, departamento: "Ventas"),
    Empleado(nombre: "María", apellido: "González", edad: 30, salario: 2000.0, departamento: "Contabilidad"),
    Empleado(nombre: "Pedro", apellido: "Rodríguez", edad: 35, salario: 2500.0, departamento: "Recursos Humanos"),
    Empleado(nombre: "Ana", apellido: "Fernández", edad: 40, salario: 3000.0, departamento: "Marketing"),
    Empleado(nombre: "Luis", apellido: "García", edad: 45, salario: 3500.0, departamento: "Producción")
]

// Definimos una función que calcula el salario total de un empleado.
func calcularSalarioTotal(empleado: Empleado) -> Double {
    return empleado.salario * 12
}

// Definimos una función que calcula la edad promedio de los empleados.
func calcularEdadPromedio(empleados: [Empleado]) -> Double {
    var edadTotal = 0
    for empleado in empleados {
        edadTotal += empleado.edad
    }
    return Double(edadTotal) / Double(empleados.count)
}

// Definimos una función que devuelve la lista de empleados ordenados por edad.
func ordenarEmpleadosPorEdad(empleados: [Empleado]) -> [Empleado] {
    return empleados.sorted { $0.edad < $1.edad }
}

// Definimos una función que devuelve la lista de empleados ordenados por salario.
func ordenarEmpleadosPorSalario(empleados: [Empleado]) -> [Empleado] {
    return empleados.sorted { $0.salario < $1.salario }
}

// Definimos una función que devuelve la lista de departamentos de los empleados.
func obtenerDepartamentos(empleados: [Empleado]) -> [String] {
    var departamentos: [String] = []
    for empleado in empleados {
        if !departamentos.contains(empleado.departamento) {
            departamentos.append(empleado.departamento)
        }
    }
    return departamentos
}

// Definimos una función que devuelve la lista de empleados de un departamento específico.
func obtenerEmpleadosPorDepartamento(empleados: [Empleado], departamento: String) -> [Empleado] {
    return empleados.filter { $0.departamento == departamento }
}

// Definimos una función que devuelve el empleado con el mayor salario.
func obtenerEmpleadoConMayorSalario(empleados: [Empleado]) -> Empleado? {
    return empleados.max(by: { $0.salario < $1.salario })
}

// Definimos una función que devuelve el empleado con la menor edad.
func obtenerEmpleadoConMenorEdad(empleados: [Empleado]) -> Empleado? {
    return empleados.min(by: { $0.edad < $1.edad })
}

// Imprimimos la lista de empleados.
print("Lista de empleados:")
for empleado in empleados {
    print("\(empleado.nombre) \(empleado.apellido) (\(empleado.edad) años) - Salario: \(empleado.salario)€ - Departamento: \(empleado.departamento)")
}

// Imprimimos la edad promedio de los empleados.
print("Edad promedio de los empleados: \(calcularEdadPromedio(empleados: empleados)) años")

// Imprimimos la lista de empleados ordenados por edad.
print("Lista de empleados ordenados por edad:")
for empleado in ordenarEmpleadosPorEdad(empleados: empleados) {
    print("\(empleado.nombre) \(empleado.apellido) (\(empleado.edad) años) - Salario: \(empleado.salario)€ - Departamento: \(empleado.departamento)")
}

// Imprimimos la lista de empleados ordenados por salario.
print("Lista de empleados ordenados por salario:")
for empleado in ordenarEmpleadosPorSalario(empleados: empleados) {
    print("\(empleado.nombre) \(empleado.apellido) (\(empleado.edad) años) - Salario: \(empleado.salario)€ - Departamento: \(empleado.departamento)")
}

// Imprimimos la lista de departamentos de los empleados.
print("Lista de departamentos:")
for departamento in obtenerDepartamentos(empleados: empleados) {
    print(departamento)
}

// Imprimimos la lista de empleados del departamento de Ventas.
print("Lista de empleados del departamento de Ventas:")
for empleado in obtenerEmpleadosPorDepartamento(empleados: empleados, departamento: "Ventas") {
    print("\(empleado.nombre) \(empleado.apellido) (\(empleado.edad) años) - Salario: \(empleado.salario)€ - Departamento: \(empleado.departamento)")
}

// Imprimimos el empleado con el mayor salario.
print("Empleado con el mayor salario:")
if let empleado = obtenerEmpleadoConMayorSalario(empleados: empleados) {
    print("\(empleado.nombre) \(empleado.apellido) (\(empleado.edad) años) - Salario: \(empleado.salario