```swift
// 1. Función para calcular el factorial de un número
func factorial(_ n: Int) -> Int? {
  if (n < 0) {
    return nil // El factorial no existe para números negativos
  } else if (n == 0) {
    return 1 // El factorial de 0 es 1
  } else {
    return (1...n).reduce(1, *) // Calcula el factorial de n mediante reduce
  }
}

// 2. Función para calcular el valor de la fórmula cuadrática
func formulaCuadratica(_ a: Double, _ b: Double, _ c: Double) -> [Double] {
  let discriminant = b*b - 4*a*c // Calcula el discriminante
  if (discriminant < 0) {
    return [] // Si el discriminante es negativo, no hay raíces reales
  } else if (discriminant == 0) {
    return [-b / (2.0 * a)] // Si el discriminante es 0, hay una raíz única
  } else {
    let root1 = (-b + sqrt(discriminant)) / (2.0 * a)
    let root2 = (-b - sqrt(discriminant)) / (2.0 * a)
    return [root1, root2] // Si el discriminante es positivo, hay dos raíces
  }
}

// 3. Función para calcular la suma de los primeros n números naturales
func sumaNaturales(_ n: Int) -> Int {
  if (n <= 0) {
    return 0 // Si n es menor o igual a 0, la suma es 0
  } else {
    return (1...n).reduce(0, +) // Calcula la suma de los primeros n números naturales
  }
}

// 4. Función para ordenar un array de cadenas en orden alfabético
func ordenarCadenas(_ array: [String]) -> [String] {
  return array.sorted(by: { (s1, s2) -> Bool in s1 < s2 }) // Ordena el array en orden alfabético
}

// 5. Función para encontrar el elemento máximo en un array de números
func maximoArray(_ array: [Int]) -> Int? {
  if (array.isEmpty) {
    return nil // Si el array está vacío, no hay máximo
  } else {
    return array.max() // Encuentra el elemento máximo en el array
  }
}

// 6. Función para calcular el área de un triángulo
func areaTriangulo(_ base: Double, _ altura: Double) -> Double {
  return 0.5 * base * altura // Calcula el área del triángulo
}

// 7. Función para calcular el área de un círculo
func areaCirculo(_ radio: Double) -> Double {
  return π * radio * radio // Calcula el área del círculo
}

// 8. Función para calcular el volumen de una esfera
func volumenEsfera(_ radio: Double) -> Double {
  return (4.0 / 3.0) * π * radio * radio * radio // Calcula el volumen de la esfera
}

// 9. Función para calcular el área de un cilindro
func areaCilindro(_ radio: Double, _ altura: Double) -> Double {
  return 2.0 * π * radio * altura + 2.0 * π * radio * radio // Calcula el área del cilindro
}

// 10. Función para calcular el volumen de un cilindro
func volumenCilindro(_ radio: Double, _ altura: Double) -> Double {
  return π * radio * radio * altura // Calcula el volumen del cilindro
}
```

Explicación del código:

* La primera función calcula el factorial de un número utilizando la función `reduce` para multiplicar todos los números desde 1 hasta n.
* La segunda función calcula las raíces de una ecuación cuadrática utilizando la fórmula cuadrática.
* La tercera función calcula la suma de los primeros n números naturales utilizando la función `reduce` para sumar todos los números desde 1 hasta n.
* La cuarta función ordena un array de cadenas en orden alfabético utilizando el método `sorted(by:)`.
* La quinta función encuentra el elemento máximo en un array de números utilizando el método `max()`.
* La sexta función calcula el área de un triángulo utilizando la fórmula para el área de un triángulo.
* La séptima función calcula el área de un círculo utilizando la fórmula para el área de un círculo.
* La octava función calcula el volumen de una esfera utilizando la fórmula para el volumen de una esfera.
* La novena función calcula el área de un cilindro utilizando la fórmula para el área de un cilindro.
* La décima función calcula el volumen de un cilindro utilizando la fórmula para el volumen de un cilindro.