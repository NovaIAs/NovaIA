```swift
// Creamos una función que devuelva un número aleatorio entre dos valores.
func generarNumeroAleatorio(rango: Rango<Int>) -> Int {
    // Obtenemos el valor mínimo y máximo del rango.
    let minimo = rango.startIndex
    let maximo = rango.endIndex - 1

    // Generamos un número aleatorio entre el valor mínimo y máximo.
    let numeroAleatorio = Int.random(in: minimo...maximo)

    // Devolvemos el número aleatorio.
    return numeroAleatorio
}

// Definimos un rango de números del 1 al 100.
let rango = 1...100

// Generamos 10 números aleatorios dentro del rango definido.
var numerosAleatorios: [Int] = []
for _ in 1...10 {
    let numeroAleatorio = generarNumeroAleatorio(rango: rango)
    numerosAleatorios.append(numeroAleatorio)
}

// Imprimimos los números aleatorios generados.
print("Números aleatorios:")
for numeroAleatorio in numerosAleatorios {
    print(numeroAleatorio)
}

// Ordenamos los números aleatorios de menor a mayor.
let numerosOrdenados = numerosAleatorios.sorted()

// Imprimimos los números aleatorios ordenados.
print("Números aleatorios ordenados:")
for numeroOrdenado in numerosOrdenados {
    print(numeroOrdenado)
}

// Buscamos el número 50 en la lista de números aleatorios ordenados.
let numeroBuscado = 50
let indiceEncontrado = numerosOrdenados.firstIndex(of: numeroBuscado)

// Si el número buscado se encuentra en la lista, imprimimos su índice.
if let indiceEncontrado = indiceEncontrado {
    print("El número \(numeroBuscado) se encuentra en la posición \(indiceEncontrado) de la lista.")
} else {
    print("El número \(numeroBuscado) no se encuentra en la lista.")
}
```

Explicación del código:

1. Creamos una función llamada `generarNumeroAleatorio` que recibe un rango de valores como parámetro y devuelve un número aleatorio dentro de ese rango.
2. Definimos un rango de números del 1 al 100.
3. Generamos 10 números aleatorios dentro del rango definido y los almacenamos en una lista.
4. Imprimimos los números aleatorios generados.
5. Ordenamos los números aleatorios de menor a mayor.
6. Imprimimos los números aleatorios ordenados.
7. Buscamos el número 50 en la lista de números aleatorios ordenados.
8. Si el número buscado se encuentra en la lista, imprimimos su índice.
9. Si el número buscado no se encuentra en la lista, imprimimos un mensaje indicando que no se ha encontrado.