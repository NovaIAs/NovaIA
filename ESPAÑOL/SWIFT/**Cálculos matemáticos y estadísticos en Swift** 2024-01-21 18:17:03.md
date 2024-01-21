```swift
// Definimos una función que genera un número aleatorio entre dos valores
func generarNumeroAleatorio(min: Int, max: Int) -> Int {
    // Generamos un número aleatorio utilizando el algoritmo arc4random_uniform
    let numeroAleatorio = Int(arc4random_uniform(UInt32(max - min + 1))) + min

    // Devolvemos el número aleatorio
    return numeroAleatorio
}

// Definimos una función que calcula el factorial de un número
func factorial(numero: Int) -> Int {
    // Si el número es 0, el factorial es 1
    if numero == 0 {
        return 1
    }

    // Si el número es mayor que 0, calculamos el factorial de forma recursiva
    else {
        return numero * factorial(numero: numero - 1)
    }
}

// Definimos una función que calcula la media de una lista de números
func media(numeros: [Int]) -> Double {
    // Calculamos la suma de todos los números de la lista
    let suma = numeros.reduce(0, +)

    // Calculamos la media dividiendo la suma por el número de elementos de la lista
    let media = Double(suma) / Double(numeros.count)

    // Devolvemos la media
    return media
}

// Definimos una función que calcula la desviación estándar de una lista de números
func desviacionEstandar(numeros: [Int]) -> Double {
    // Calculamos la media de la lista
    let media = self.media(numeros: numeros)

    // Calculamos la varianza de la lista
    let varianza = numeros.map { Double($0 - media) * Double($0 - media) }.reduce(0, +) / Double(numeros.count)

    // Calculamos la desviación estándar de la lista
    let desviacionEstandar = sqrt(varianza)

    // Devolvemos la desviación estándar
    return desviacionEstandar
}

// Definimos una función que calcula la correlación entre dos listas de números
func correlacion(numeros1: [Int], numeros2: [Int]) -> Double {
    // Calculamos la media de cada lista
    let media1 = media(numeros: numeros1)
    let media2 = media(numeros: numeros2)

    // Calculamos la covarianza de las dos listas
    let covarianza = numeros1.map { Double($0 - media1) * Double($0 - media2) }.reduce(0, +) / Double(numeros1.count)

    // Calculamos la desviación estándar de cada lista
    let desviacionEstandar1 = desviacionEstandar(numeros: numeros1)
    let desviacionEstandar2 = desviacionEstandar(numeros: numeros2)

    // Calculamos la correlación entre las dos listas
    let correlacion = covarianza / (desviacionEstandar1 * desviacionEstandar2)

    // Devolvemos la correlación
    return correlacion
}

// Definimos una función que genera una lista de números aleatorios
func generarListaNumerosAleatorios(longitud: Int, min: Int, max: Int) -> [Int] {
    // Creamos una lista vacía para almacenar los números aleatorios
    var listaNumerosAleatorios = [Int]()

    // Generamos un número aleatorio para cada elemento de la lista
    for _ in 0..<longitud {
        let numeroAleatorio = generarNumeroAleatorio(min: min, max: max)
        listaNumerosAleatorios.append(numeroAleatorio)
    }

    // Devolvemos la lista de números aleatorios
    return listaNumerosAleatorios
}

// Definimos una función que imprime los elementos de una lista
func imprimirLista(lista: [Int]) {
    // Recorremos la lista y imprimimos cada elemento
    for numero in lista {
        print(numero)
    }

    // Imprimimos una línea en blanco para separar las listas
    print()
}

// Generamos una lista de números aleatorios
let listaNumerosAleatorios1 = generarListaNumerosAleatorios(longitud: 10, min: 0, max: 100)

// Imprimimos la lista de números aleatorios
print("Lista de números aleatorios 1:")
imprimirLista(lista: listaNumerosAleatorios1)

// Generamos otra lista de números aleatorios
let listaNumerosAleatorios2 = generarListaNumerosAleatorios(longitud: 10, min: 0, max: 100)

// Imprimimos la lista de números aleatorios
print("Lista de números aleatorios 2:")
imprimirLista(lista: listaNumerosAleatorios1)

// Calculamos la media de cada lista
let media1 = media(numeros: listaNumerosAleatorios1)
let media2 = media(numeros: listaNumerosAleatorios2)

// Imprimimos las medias de las listas
print("Media de la lista 1:", media1)
print("Media de la lista 2:", media2)

// Calculamos la desviación estándar de cada lista
let desviacionEstandar1 = desviacionEstandar(numeros: listaNumerosAleatorios1)
let desviacionEstandar2 = desviacionEstandar(numeros: listaNumerosAleatorios2)

// Imprimimos las desviaciones estándar de las listas
print("Desviación estándar de la lista 1:", desviacionEstandar1)
print("Desviación estándar de la lista 2:", desviacionEstandar2)

// Calculamos la correlación entre las dos listas
let correlacion = correlacion(numeros1: listaNumerosAleatorios1, numeros2: listaNumerosAleatorios2)

// Imprimimos la correlación entre las dos listas
print("Correlación entre las dos listas:", correlacion)
```

Explicación:

* El código define varias funciones para realizar cálculos matemáticos y estadísticos, como generar números aleatorios, calcular el factorial de un número, calcular la media, la desviación estándar y la correlación de una lista de números.
* El código también define una función para generar una lista de números aleatorios y una función para imprimir los elementos de una lista.
* El código genera dos listas de números aleatorios, calcula la media, la desviación estándar y la correlación de cada lista, y luego imprime los resultados.
* El código utiliza la biblioteca estándar de Swift para realizar los cálculos matemáticos y estadísticos.