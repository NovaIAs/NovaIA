```swift
// Este es un comentario de una línea que explica el propósito del código.

// Esta es una función que imprime "Hola mundo" en la consola.
func holaMundo() {
  print("Hola mundo")
}

// Esta es una clase que representa un perro.
class Perro {
  // Estas son las propiedades de la clase Perro.
  var nombre: String
  var edad: Int
  var raza: String

  // Este es el constructor de la clase Perro.
  init(nombre: String, edad: Int, raza: String) {
    self.nombre = nombre
    self.edad = edad
    self.raza = raza
  }

  // Este es un método de la clase Perro que imprime el nombre, la edad y la raza del perro.
  func ladrar() {
    print("\(nombre) es un perro de raza \(raza) de \(edad) años.")
  }
}

// Esta es una instancia de la clase Perro.
let perro = Perro(nombre: "Firulais", edad: 5, raza: "Golden Retriever")

// Este es un bucle que imprime los números del 1 al 10.
for i in 1...10 {
  print(i)
}

// Este es un bucle que imprime los números del 10 al 1.
for i in stride(from: 10, to: 1, by: -1) {
  print(i)
}

// Este es un bucle que imprime los elementos de una lista.
let lista = ["uno", "dos", "tres"]
for elemento in lista {
  print(elemento)
}

// Este es un bucle que imprime los elementos de un diccionario.
let diccionario = ["uno": 1, "dos": 2, "tres": 3]
for (clave, valor) in diccionario {
  print("\(clave): \(valor)")
}

// Este es un bucle que imprime los elementos de un conjunto.
let conjunto = Set([1, 2, 3])
for elemento in conjunto {
  print(elemento)
}

// Este es un condicional que imprime "Hola" si la variable `x` es mayor que 0.
let x = 1
if x > 0 {
  print("Hola")
} else {
  print("Adiós")
}

// Este es un condicional que imprime "Hola" si la variable `y` es mayor que 0, y "Adiós" en caso contrario.
let y = -1
if y > 0 {
  print("Hola")
} else if y < 0 {
  print("Adiós")
} else {
  print("Neutro")
}

// Este es un condicional que imprime "Hola" si la variable `z` es mayor que 0, "Adiós" si es menor que 0, y "Neutro" si es igual a 0.
let z = 0
switch z {
case 1...Int.max:
  print("Hola")
case Int.min...(-1):
  print("Adiós")
default:
  print("Neutro")
}

// Esta es una función que recibe un número y devuelve su factorial.
func factorial(_ n: Int) -> Int {
  if n == 1 {
    return 1
  } else {
    return n * factorial(n - 1)
  }
}

// Este es un ejemplo de uso de la función `factorial`.
print(factorial(5)) // 120

// Esta es una función que calcula el máximo común divisor de dos números.
func mcd(_ m: Int, _ n: Int) -> Int {
  if n == 0 {
    return m
  } else {
    return mcd(n, m % n)
  }
}

// Este es un ejemplo de uso de la función `mcd`.
print(mcd(21, 14)) // 7

// Esta es una función que calcula el mínimo común múltiplo de dos números.
func mcm(_ m: Int, _ n: Int) -> Int {
  return m * n / mcd(m, n)
}

// Este es un ejemplo de uso de la función `mcm`.
print(mcm(21, 14)) // 42

// Este es un array que contiene los números del 1 al 10.
let array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

// Este es un diccionario que contiene los nombres de los planetas y sus distancias al Sol.
let diccionario = ["Mercurio": 57.9, "Venus": 108.2, "Tierra": 149.6, "Marte": 227.9, "Júpiter": 778.6, "Saturno": 1429.8, "Urano": 2871.0, "Neptuno": 4503.5]

// Este es un conjunto que contiene los colores del arcoíris.
let conjunto = Set(["rojo", "naranja", "amarillo", "verde", "azul", "añil", "violeta"])

// Este es un ejemplo de uso de la función `holaMundo`.
holaMundo() // Imprime "Hola mundo" en la consola.

// Este es un ejemplo de uso de la clase `Perro`.
perro.ladrar() // Imprime "Firulais es un perro de raza Golden Retriever de 5 años."

// Este es un ejemplo de uso del bucle `for`.
for i in 1...10 {
  print(i) // Imprime los números del 1 al 10 en la consola.
}

// Este es un ejemplo de uso del condicional `if`.
if x > 0 {
  print("Hola") // Imprime "Hola" en la consola porque x es mayor que 0.
} else {
  print("Adiós") // No se imprime nada porque x no es menor o igual que 0.
}

// Este es un ejemplo de uso del condicional `switch`.
switch z {
case 1...Int.max:
  print("Hola") // No se imprime nada porque z no es mayor que 0.
case Int.min...(-1):
  print("Adiós") // No se imprime nada porque z no es menor que 0.
default:
  print("Neutro") // Imprime "Neutro" en la consola porque z es igual a 0.
}

// Este es un ejemplo de uso de la función `factorial`.
print(factorial(5)) // Imprime 120 en la consola.

// Este es un ejemplo de uso de la función `mcd`.
print(mcd(21, 14)) // Imprime 7 en la consola.

// Este es un ejemplo de uso de la función `mcm`.
print(mcm(21, 14)) // Imprime 42 en la consola.

// Este es un ejemplo de uso del array `array`.
print(array) // Imprime el array en la consola.

// Este es un ejemplo de uso del diccionario `diccionario`.
print(diccionario) // Imprime el diccionario en la consola.

// Este es un ejemplo de uso del conjunto `conjunto`.
print(conjunto) // Imprime el conjunto en la consola.
```

Este código es un conjunto de funciones, clases, bucles, condicionales y estructuras de datos que ilustran una variedad de características del lenguaje de programación Swift. El código está bien documentado con comentarios que explican cada parte del código en detalle.

El código incluye funciones para calcular el factorial de un número, el máximo común divisor de dos números y el mínimo común múltiplo de dos números. También incluye una clase que representa a un perro con propiedades para su nombre, edad y raza, y un método para imprimir su información.

El código utiliza bucles para iterar sobre arrays, diccionarios y conjuntos, y utiliza condicionales para tomar decisiones. El código también utiliza una variedad de estructuras de datos, incluyendo arrays, diccionarios y conjuntos.

Este código es un ejemplo complejo y detallado de cómo utilizar el lenguaje de programación Swift.