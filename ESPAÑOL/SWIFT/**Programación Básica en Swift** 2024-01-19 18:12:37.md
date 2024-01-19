```swift
// Variables y Constantes

// Definimos una variable de tipo entero llamada "numero" y le asignamos el valor 10.
var numero = 10

// Definimos una constante de tipo cadena de caracteres llamada "nombre" y le asignamos el valor "Juan".
let nombre = "Juan"

// Operadores y Expresiones

// Realizamos una operación aritmética para sumar dos números.
let suma = 10 + 5

// Realizamos una operación lógica para comprobar si un número es mayor que otro.
let esMayor = 10 > 5

// Estructuras de Control

// Estructura de control if-else para tomar decisiones.
if numero > 10 {
    print("El número es mayor que 10")
} else {
    print("El número es menor o igual que 10")
}

// Estructura de control switch para evaluar múltiples casos.
switch numero {
case 10:
    print("El número es 10")
case 20:
    print("El número es 20")
case 30:
    print("El número es 30")
default:
    print("El número no es 10, 20 ni 30")
}

// Bucles

// Bucle for para iterar sobre una colección.
for i in 1...10 {
    print("El valor de i es \(i)")
}

// Bucle while para iterar hasta que una condición sea falsa.
var contador = 0
while contador < 10 {
    print("El valor de contador es \(contador)")
    contador += 1
}

// Funciones

// Definimos una función que no recibe argumentos y devuelve un valor de tipo entero.
func sumar(a: Int, b: Int) -> Int {
    return a + b
}

// Invocamos la función sumar y le pasamos dos argumentos.
let resultado = sumar(a: 10, b: 20)
print("El resultado de la suma es \(resultado)")

// Clases y Objetos

// Definimos una clase llamada Persona.
class Persona {
    var nombre: String
    var edad: Int

    init(nombre: String, edad: Int) {
        self.nombre = nombre
        self.edad = edad
    }

    func saludar() {
        print("Hola, mi nombre es \(nombre) y tengo \(edad) años.")
    }
}

// Creamos un objeto de la clase Persona.
let persona = Persona(nombre: "Juan", edad: 25)

// Llamamos al método saludar del objeto persona.
persona.saludar()

// Arrays

// Definimos un array de números enteros.
let numeros = [1, 2, 3, 4, 5]

// Accedemos al primer elemento del array.
let primerNumero = numeros[0]

// Recorremos el array utilizando un bucle for.
for numero in numeros {
    print("El número es \(numero)")
}

// Diccionarios

// Definimos un diccionario de cadenas de caracteres a valores enteros.
let diccionario = ["uno": 1, "dos": 2, "tres": 3]

// Accedemos al valor asociado a una clave.
let valor = diccionario["uno"]

// Recorremos el diccionario utilizando un bucle for.
for (clave, valor) in diccionario {
    print("La clave es \(clave) y el valor es \(valor)")
}

// Conjuntos

// Definimos un conjunto de cadenas de caracteres.
let conjunto = Set(["uno", "dos", "tres"])

// Comprobamos si un elemento pertenece al conjunto.
let contieneUno = conjunto.contains("uno")

// Recorremos el conjunto utilizando un bucle for.
for elemento in conjunto {
    print("El elemento es \(elemento)")
}

// Gestión de Errores

// Definimos una función que puede lanzar una excepción.
func dividir(a: Int, b: Int) throws -> Int {
    if b == 0 {
        throw NSError(domain: "com.example.app", code: 1, userInfo: nil)
    }

    return a / b
}

// Intentamos llamar a la función dividir y manejamos el error.
do {
    let resultado = try dividir(a: 10, b: 2)
    print("El resultado de la división es \(resultado)")
} catch {
    print("Ocurrió un error al dividir: \(error)")
}
```

**Explicación del código:**

* **Variables y Constantes:** Definimos variables y constantes para almacenar datos. Las variables pueden cambiar su valor, mientras que las constantes no.
* **Operadores y Expresiones:** Utilizamos operadores para realizar operaciones aritméticas y lógicas. Las expresiones son combinaciones de variables, constantes, operadores y paréntesis que producen un valor.
* **Estructuras de Control:** Utilizamos estructuras de control como if-else, switch y bucles for y while para tomar decisiones y controlar el flujo del programa.
* **Funciones:** Las funciones son bloques de código que pueden recibir argumentos y devolver un valor. Las funciones pueden reutilizarse en diferentes partes del código.
* **Clases y Objetos:** Las clases son plantillas para crear objetos. Los objetos son instancias de una clase y tienen sus propias propiedades y métodos.
* **Arrays:** Los arrays son colecciones ordenadas de elementos. Podemos acceder a los elementos de un array mediante su índice.
* **Diccionarios:** Los diccionarios son colecciones no ordenadas de pares clave-valor. Podemos acceder al valor asociado a una clave mediante la clave.
* **Conjuntos:** Los conjuntos son colecciones no ordenadas de elementos únicos. Podemos comprobar si un elemento pertenece a un conjunto utilizando la función contains.
* **Gestión de Errores:** Utilizamos la gestión de errores para manejar errores que pueden ocurrir durante la ejecución del programa. Podemos utilizar la palabra clave do-catch para intentar ejecutar un bloque de código y manejar el error si ocurre.

Este código es un ejemplo de cómo utilizar las diferentes características del lenguaje Swift para crear un programa complejo y diferenciado.