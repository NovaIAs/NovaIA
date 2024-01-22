```swift
// Función para calcular el máximo común divisor (MCD) de dos números
func mcd(a: Int, b: Int) -> Int {
  if b == 0 {
    return a
  } else {
    return mcd(a: b, b: a % b)
  }
}

// Función para generar una lista de números primos
func primos(hasta: Int) -> [Int] {
    var primos = [Int]()
    for i in 2...hasta {
        var esPrimo = true
        for j in 2..<i {
            if i % j == 0 {
                esPrimo = false
            }
        }
        if esPrimo {
            primos.append(i)
        }
    }
    return primos
}

// Función para encontrar todos los factores primos de un número
func factoresPrimos(numero: Int) -> [Int] {
    var factoresPrimos = [Int]()
    for i in 2...numero {
        while numero % i == 0 {
            factoresPrimos.append(i)
            numero /= i
        }
    }
    return factoresPrimos
}

// Función para calcular la suma de los dígitos de un número
func sumaDigitos(numero: Int) -> Int {
  var suma = 0
  var n = numero
  while n > 0 {
    suma += n % 10
    n /= 10
  }
  return suma
}

// Función para invertir un número
func invertir(numero: Int) -> Int {
  var invertido = 0
  var n = numero
  while n > 0 {
    invertido = invertido * 10 + (n % 10)
    n /= 10
  }
  return invertido
}

// Función para verificar si un número es capicúa (palíndromo)
func esCapicua(numero: Int) -> Bool {
  return numero == invertir(numero: numero)
}

// Función para calcular el número de Armstrong de un número
func armstrong(numero: Int) -> Int {
  var armstrong = 0
  var n = numero
  var numDigitos = 0
  while n > 0 {
    numDigitos += 1
    n /= 10
  }
  n = numero
  while n > 0 {
    armstrong += Int(pow(Double(n % 10), Double(numDigitos)))
    n /= 10
  }
  return armstrong
}

// Función para verificar si un número es perfecto
func esPerfecto(numero: Int) -> Bool {
  var sumaDivisores = 0
  for i in 1..<numero {
    if numero % i == 0 {
      sumaDivisores += i
    }
  }
  return sumaDivisores == numero
}

// Función para calcular el número perfecto más cercano a un número dado
func perfectoCercano(numero: Int) -> Int {
  var perfectoCercano = numero
  while !esPerfecto(numero: perfectoCercano) {
    perfectoCercano += 1
  }
  return perfectoCercano
}

// Función para calcular la secuencia de Fibonacci
func fibonacci(n: Int) -> Int {
  if n <= 1 {
    return n
  } else {
    return fibonacci(n: n - 1) + fibonacci(n: n - 2)
  }
}

// Función para generar una lista de números de Fibonacci
func fibonacci(hasta: Int) -> [Int] {
  var fibonacci = [0, 1]
  while fibonacci[fibonacci.count - 1] < hasta {
    let siguiente = fibonacci[fibonacci.count - 1] + fibonacci[fibonacci.count - 2]
    fibonacci.append(siguiente)
  }
  return fibonacci
}

// Función para calcular el factorial de un número
func factorial(n: Int) -> Int {
  if n == 0 {
    return 1
  } else {
    return n * factorial(n: n - 1)
  }
}

// Función para generar una lista de números factoriales
func factoriales(hasta: Int) -> [Int] {
  var factoriales = [1]
  for i in 1...hasta {
    factoriales.append(i * factoriales[i - 1])
  }
  return factoriales
}

// Función para calcular el número de combinaciones de n elementos tomados de k en k
func combinaciones(n: Int, k: Int) -> Int {
  return factorial(n: n) / (factorial(n: k) * factorial(n: n - k))
}

// Función para calcular el número de permutaciones de n elementos tomados de k en k
func permutaciones(n: Int, k: Int) -> Int {
  return factorial(n: n) / factorial(n: n - k)
}

// Función para generar una lista de todas las permutaciones de una lista
func permutaciones<T>(lista: [T]) -> [[T]] {
  var permutaciones = [[T]]()
  if lista.count == 1 {
    permutaciones.append(lista)
  } else {
    for i in 0..<lista.count {
      let elemento = lista[i]
      let sublista = lista.filter { $0 != elemento }
      let subpermutaciones = permutaciones(lista: sublista)
      for subpermutacion in subpermutaciones {
        permutaciones.append([elemento] + subpermutacion)
      }
    }
  }
  return permutaciones
}

// Función para generar una lista de todas las combinaciones de una lista
func combinaciones<T>(lista: [T], k: Int) -> [[T]] {
  var combinaciones = [[T]]()
  if k == 1 {
    for elemento in lista {
      combinaciones.append([elemento])
    }
  } else {
    for i in 0..<lista.count {
      let elemento = lista[i]
      let sublista = lista.filter { $0 != elemento }
      let subcombinaciones = combinaciones(lista: sublista, k: k - 1)
      for subcombinacion in subcombinaciones {
        combinaciones.append([elemento] + subcombinacion)
      }
    }
  }
  return combinaciones
}

// Función para generar una lista de todas las particiones de un entero
func particiones(numero: Int) -> [[Int]] {
  var particiones = [[Int]]()
  for i in 1...numero {
    let subparticiones = particiones(numero: numero - i)
    for subparticion in subparticiones {
      particiones.append([i] + subparticion)
    }
  }
  return particiones
}

// Función para generar una lista de todas las soluciones a un problema de mochila
func mochila(pesos: [Int], valores: [Int], capacidad: Int) -> [[Int]] {
  var mochila = [[Int]]()
  for i in 0..<pesos.count {
    let peso = pesos[i]
    let valor = valores[i]
    let submochila = mochila(pesos: pesos.filter
