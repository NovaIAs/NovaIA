```swift
// 1. Usando Colecciones:

// Crear un diccionario con claves de tipo String y valores de tipo Int
var edades = ["Juan": 25, "María": 30, "Pedro": 35]

// Iterar sobre el diccionario y acumular la edad promedio
var sumaEdades = 0
for (nombre, edad) in edades {
    sumaEdades += edad
}

// Calcular la edad promedio
let edadPromedio = Double(sumaEdades) / Double(edades.count)

// 2. Usando Funciones:

// Definir una función que calcule la edad promedio de un diccionario de edades
func calcularEdadPromedio(edades: [String: Int]) -> Double {
    // Acumular la edad promedio
    var sumaEdades = 0

    // Iterar sobre el diccionario y acumular la edad promedio
    for (_, edad) in edades {
        sumaEdades += edad
    }

    // Calcular la edad promedio
    let edadPromedio = Double(sumaEdades) / Double(edades.count)

    // Devolver la edad promedio
    return edadPromedio
}

// Llamar la función para calcular la edad promedio
let edadPromedio = calcularEdadPromedio(edades: edades)

// 3. Usando Programación Funcional:

// Crear una función que calcule la edad promedio de un arreglo de enteros
let calcularEdadPromedio = { (edades: [Int]) -> Double in
    // Acumular la edad promedio
    var sumaEdades = 0

    // Iterar sobre el arreglo y acumular la edad promedio
    for edad in edades {
        sumaEdades += edad
    }

    // Calcular la edad promedio
    let edadPromedio = Double(sumaEdades) / Double(edades.count)

    // Devolver la edad promedio
    return edadPromedio
}

// Llamar la función para calcular la edad promedio
let edadPromedio = calcularEdadPromedio([25, 30, 35])

// 4. Usando Programación Imperativa:

// Crear un arreglo de enteros con las edades
var edades = [25, 30, 35]

// Acumular la edad promedio
var sumaEdades = 0

// Iterar sobre el arreglo y acumular la edad promedio
for edad in edades {
    sumaEdades += edad
}

// Calcular la edad promedio
let edadPromedio = Double(sumaEdades) / Double(edades.count)

// 5. Usando Closures:

// Crear un arreglo de enteros con las edades
var edades = [25, 30, 35]

// Crear un closure que calcule la edad promedio
let calcularEdadPromedio = { (edades: [Int]) -> Double in
    // Acumular la edad promedio
    var sumaEdades = 0

    // Iterar sobre el arreglo y acumular la edad promedio
    for edad in edades {
        sumaEdades += edad
    }

    // Calcular la edad promedio
    let edadPromedio = Double(sumaEdades) / Double(edades.count)

    // Devolver la edad promedio
    return edadPromedio
}

// Llamar al closure para calcular la edad promedio
let edadPromedio = calcularEdadPromedio(edades)

// 6. Usando Estructuras:

// Crear una estructura para representar una persona con su nombre y edad
struct Persona {
    var nombre: String
    var edad: Int
}

// Crear un arreglo de personas
var personas = [
    Persona(nombre: "Juan", edad: 25),
    Persona(nombre: "María", edad: 30),
    Persona(nombre: "Pedro", edad: 35)
]

// Calcular la edad promedio
let edadPromedio = personas.reduce(0) { (suma, persona) in suma + persona.edad } / Double(personas.count)

// 7. Usando Protocolos:

// Crear un protocolo para representar un objeto que pueda calcular la edad promedio
protocol CalculableEdadPromedio {
    func calcularEdadPromedio() -> Double
}

// Extender el protocolo CalculableEdadPromedio a la estructura Persona
extension Persona: CalculableEdadPromedio {
    func calcularEdadPromedio() -> Double {
        // Calcular la edad promedio
        return Double(edad)
    }
}

// Crear un arreglo de objetos que implementen el protocolo CalculableEdadPromedio
var objetos = [
    Persona(nombre: "Juan", edad: 25),
    Persona(nombre: "María", edad: 30),
    Persona(nombre: "Pedro", edad: 3