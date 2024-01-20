```swift
// Definir una función para calcular el factorial de un número entero

func factorial(_ número: Int) -> Int {
  var resultado = 1
  for i in 1...número {
    resultado *= i
  }
  return resultado
}

// Definir una función para encontrar el máximo común divisor (MCD) de dos números enteros

func mcd(_ número1: Int, _ número2: Int) -> Int {
  var resto = número1 % número2
  while resto != 0 {
    número1 = número2
    número2 = resto
    resto = número1 % número2
  }
  return número2
}

// Definir una función para encontrar el mínimo común múltiplo (MCM) de dos números enteros

func mcm(_ número1: Int, _ número2: Int) -> Int {
  return (número1 * número2) / mcd(número1, número2)
}

// Definir un algoritmo de ordenamiento rápido (quicksort) en Swift

func quicksort(_ array: [Int]) -> [Int] {
  guard array.count > 1 else {
    return array
  }

  let pivot = array[array.count / 2]
  var izquierda = [Int]()
  var derecha = [Int]()

  for elemento in array {
    if elemento < pivot {
      izquierda.append(elemento)
    } else if elemento > pivot {
      derecha.append(elemento)
    }
  }

  return quicksort(izquierda) + [pivot] + quicksort(derecha)
}

// Definir una función para realizar una búsqueda binaria en un array ordenado

func busquedaBinaria(_ array: [Int], _ objetivo: Int) -> Int {
  var bajo = 0
  var alto = array.count - 1

  while bajo <= alto {
    let medio = (bajo + alto) / 2
    let elementoMedio = array[medio]

    if elementoMedio == objetivo {
      return medio
    } else if elementoMedio < objetivo {
      bajo = medio + 1
    } else {
      alto = medio - 1
    }
  }

  return -1
}

// Definir una función para generar una secuencia de Fibonacci

func fibonacci(_ hasta: Int) -> [Int] {
  var secuencia = [0, 1]

  while secuencia.count < hasta {
    let siguiente = secuencia[secuencia.count - 1] + secuencia[secuencia.count - 2]
    secuencia.append(siguiente)
  }

  return secuencia
}

// Ejemplo de uso de las funciones definidas

let factorialDe5 = factorial(5)  // Resultado: 120
let mcdDe12Y18 = mcd(12, 18)  // Resultado: 6
let mcmDe12Y18 = mcm(12, 18)  // Resultado: 36

let arrayDesordenado = [5, 3, 1, 2, 4]
let arrayOrdenado = quicksort(arrayDesordenado)  // Resultado: [1, 2, 3, 4, 5]

let objetivo = 3
let índiceObjetivo = busquedaBinaria(arrayOrdenado, objetivo)  // Resultado: 2

let secuenciaFibonacci = fibonacci(10)  // Resultado: [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

print("Factorial de 5: \(factorialDe5)")
print("MCD de 12 y 18: \(mcdDe12Y18)")
print("MCM de 12 y 18: \(mcmDe12Y18)")

print("Array ordenado: \(arrayOrdenado)")
print("Índice del objetivo en el array ordenado: \(índiceObjetivo)")

print("Secuencia Fibonacci hasta 10: \(secuenciaFibonacci)")
```

Explicación del código:

- La primera función, `factorial`, calcula el factorial de un número entero positivo utilizando un bucle `for` para multiplicar los números desde 1 hasta el número dado.


- La segunda función, `mcd`, calcula el máximo común divisor de dos números enteros positivos utilizando el algoritmo de Euclides.


- La tercera función, `mcm`, calcula el mínimo común múltiplo de dos números enteros positivos utilizando la fórmula `mcm(a, b) = (a * b) / mcd(a, b)`.


- La cuarta función, `quicksort`, implementa el algoritmo de ordenamiento rápido para ordenar un array de números enteros en orden ascendente.


- La quinta función, `busquedaBinaria`, implementa el algoritmo de búsqueda binaria para encontrar la posición de un elemento objetivo en un array ordenado.


- La sexta función, `fibonacci`, genera una secuencia de Fibonacci hasta un número límite dado.


- El código principal utiliza estas funciones para realizar algunos cálculos y mostrar los resultados.