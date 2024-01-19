**Código Complejo en Swift:**

**1. Clases y Estructuras:**

```swift
// Define una clase llamada "Persona" con propiedades "nombre" y "edad".
class Persona {
    var nombre: String
    var edad: Int

    // Inicializador de la clase "Persona".
    init(nombre: String, edad: Int) {
        self.nombre = nombre
        self.edad = edad
    }

    // Método para imprimir la información de la persona.
    func imprimirInfo() {
        print("Nombre: \(nombre), Edad: \(edad)")
    }
}

// Define una estructura llamada "Dirección" con propiedades "calle", "número", "ciudad" y "código_postal".
struct Dirección {
    var calle: String
    var número: Int
    var ciudad: String
    var código_postal: Int

    // Inicializador de la estructura "Dirección".
    init(calle: String, número: Int, ciudad: String, código_postal: Int) {
        self.calle = calle
        self.número = número
        self.ciudad = ciudad
        self.código_postal = código_postal
    }

    // Método para imprimir la información de la dirección.
    func imprimirInfo() {
        print("Calle: \(calle), Número: \(número), Ciudad: \(ciudad), Código Postal: \(código_postal)")
    }
}
```

**2. Arrays y Diccionarios:**

```swift
// Define un array de objetos "Persona".
var personas: [Persona] = [
    Persona(nombre: "Juan", edad: 25),
    Persona(nombre: "María", edad: 30),
    Persona(nombre: "Pedro", edad: 35)
]

// Define un diccionario de objetos "Dirección".
var direcciones: [String: Dirección] = [
    "Juan": Dirección(calle: "Calle Mayor", número: 123, ciudad: "Madrid", código_postal: 28013),
    "María": Dirección(calle: "Calle Menor", número: 456, ciudad: "Barcelona", código_postal: 08015),
    "Pedro": Dirección(calle: "Calle Nueva", número: 789, ciudad: "Valencia", código_postal: 46001)
]
```

**3. Bucles y Condicionales:**

```swift
// Recorre el array de personas y llama al método "imprimirInfo()" para cada persona.
for persona in personas {
    persona.imprimirInfo()
}

// Recorre el diccionario de direcciones y llama al método "imprimirInfo()" para cada dirección.
for (nombre, direccion) in direcciones {
    print("Dirección de \(nombre):")
    direccion.imprimirInfo()
}

// Comprueba si la lista de personas contiene una persona con el nombre "Pedro".
if let persona = personas.first(where: { $0.nombre == "Pedro" }) {
    print("Se ha encontrado a Pedro en la lista de personas.")
} else {
    print("No se ha encontrado a Pedro en la lista de personas.")
}
```

**4. Opcionales y Manejo de Errores:**

```swift
// Define una variable opcional para almacenar un número entero.
var numero: Int?

// Comprueba si la variable "numero" tiene un valor.
if let numero = numero {
    // Si la variable "numero" tiene un valor, lo imprime.
    print("El valor de la variable 'numero' es \(numero).")
} else {
    // Si la variable "numero" no tiene un valor, imprime un mensaje de error.
    print("La variable 'numero' no tiene un valor.")
}

// Define una función que puede lanzar una excepción.
func dividir(numerador: Int, denominador: Int) throws -> Int {
    // Comprueba si el denominador es cero. Si lo es, lanza una excepción.
    guard denominador != 0 else {
        throw NSError(domain: "com.ejemplo.errores", code: 1, userInfo: nil)
    }

    // Divide el numerador por el denominador y devuelve el resultado.
    return numerador / denominador
}

// Intenta llamar a la función "dividir()" y maneja cualquier excepción que pueda lanzarse.
do {
    let resultado = try dividir(numerador: 10, denominador: 2)
    print("El resultado de la división es \(resultado).")
} catch {
    // Si se lanza una excepción, imprime un mensaje de error.
    print("Se ha producido un error al dividir: \(error).")
}
```

**5. Protocolos y Extensiones:**

```swift
// Define un protocolo llamado "Imprimible".
protocol Imprimible {
    func imprimirInfo()
}

// Extiende el protocolo "Imprimible" a la clase "Persona".
extension Persona: Imprimible {
    func imprimirInfo() {
        print("Persona: \(nombre), Edad: \(edad)")
    }
}

// Extiende el protocolo "Imprimible" a la estructura "Dirección".
extension Dirección: Imprimible {
    func imprimirInfo() {
        print("Calle: \(calle), Número: \(número), Ciudad: \(ciudad), Código Postal: \(código_postal)")
    }
}

// Define una función que recibe un objeto conforme al protocolo "Imprimible" y llama al método "imprimirInfo()".
func imprimirInfo(_ imprimible: Imprimible) {
    imprimible.imprimirInfo()
}

// Llama a la función "imprimirInfo()" pasando una instancia de la clase "Persona" y una instancia de la estructura "Dirección".
imprimirInfo(Persona(nombre: "Juan", edad: 25))
imprimirInfo(Dirección(calle: "Calle Mayor", número: 123, ciudad: "Madrid", código_postal: 28013))
```

**Explicación:**

Este código es un ejemplo de cómo crear clases, estructuras, arrays, diccionarios, bucles, condicionales, opcionales, manejo de errores, protocolos y extensiones en Swift. El código está completamente en español y está diseñado para ser complejo y difícil de repetir.