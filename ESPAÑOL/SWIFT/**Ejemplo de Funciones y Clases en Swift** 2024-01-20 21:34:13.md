```swift
// Definimos una función que recibe un array de números y devuelve la suma de todos ellos.
func sumarArray(array: [Int]) -> Int {
    var suma = 0
    for numero in array {
        suma += numero
    }
    return suma
}

// Definimos una clase llamada Persona que tiene un nombre y una edad.
class Persona {
    var nombre: String
    var edad: Int

    // Inicializador de la clase Persona.
    init(nombre: String, edad: Int) {
        self.nombre = nombre
        self.edad = edad
    }

    // Función que devuelve el nombre de la persona.
    func getNombre() -> String {
        return nombre
    }

    // Función que devuelve la edad de la persona.
    func getEdad() -> Int {
        return edad
    }
}

// Creamos un array de números.
let arrayNumeros = [1, 2, 3, 4, 5]

// Llamamos a la función sumarArray para obtener la suma de los números del array.
let sumaArray = sumarArray(array: arrayNumeros)

// Creamos una instancia de la clase Persona.
let persona = Persona(nombre: "Juan", edad: 20)

// Obtenemos el nombre y la edad de la persona.
let nombrePersona = persona.getNombre()
let edadPersona = persona.getEdad()

// Imprimimos los resultados.
print("La suma del array es: \(sumaArray)")
print("El nombre de la persona es: \(nombrePersona)")
print("La edad de la persona es: \(edadPersona)")
```

Explicación del código:

* La función `sumarArray` recibe un array de números y devuelve la suma de todos ellos. La función recorre el array utilizando un bucle `for` y va sumando cada número al resultado.
* La clase `Persona` tiene dos propiedades: `nombre` y `edad`. La clase también tiene un inicializador que recibe el nombre y la edad y los asigna a las propiedades. La clase también tiene dos métodos: `getNombre` y `getEdad` que devuelven el nombre y la edad de la persona, respectivamente.
* Creamos un array de números y llamamos a la función `sumarArray` para obtener la suma de los números del array.
* Creamos una instancia de la clase `Persona` y obtenemos el nombre y la edad de la persona.
* Imprimimos los resultados.