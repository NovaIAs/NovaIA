```swift
// Esta función toma una lista de números y devuelve su suma.
func suma(números: [Int]) -> Int {
    var suma = 0
    for número in números {
        suma += número
    }
    return suma
}

// Esta función toma una lista de números y devuelve su producto.
func producto(números: [Int]) -> Int {
    var producto = 1
    for número in números {
        producto *= número
    }
    return producto
}

// Esta función toma una lista de números y devuelve su media.
func media(números: [Int]) -> Double {
    return Double(suma(números: números)) / Double(números.count)
}

// Esta función toma una lista de números y devuelve su mediana.
func mediana(números: [Int]) -> Double {
    let númerosOrdenados = números.sorted()
    let mitad = númerosOrdenados.count / 2
    if númerosOrdenados.count % 2 == 0 {
        return Double(númerosOrdenados[mitad - 1] + númerosOrdenados[mitad]) / 2.0
    } else {
        return Double(númerosOrdenados[mitad])
    }
}

// Esta función toma una lista de números y devuelve su moda.
func moda(números: [Int]) -> Int? {
    var moda: Int? = nil
    var frecuenciaMáxima = 0
    var frecuencias: [Int: Int] = [:]
    for número in números {
        if let frecuencia = frecuencias[número] {
            frecuencias[número] = frecuencia + 1
        } else {
            frecuencias[número] = 1
        }
        if frecuencias[número]! > frecuenciaMáxima {
            frecuenciaMáxima = frecuencias[número]!
            moda = número
        }
    }
    return moda
}

// Esta función toma una lista de números y devuelve su rango.
func rango(números: [Int]) -> Int {
    let númerosOrdenados = números.sorted()
    return númerosOrdenados.last! - númerosOrdenados.first!
}

// Esta función toma una lista de números y devuelve su varianza.
func varianza(números: [Int]) -> Double {
    let media = Double(suma(números: números)) / Double(números.count)
    var varianza = 0.0
    for número in números {
        varianza += pow(Double(número) - media, 2)
    }
    return varianza / Double(números.count - 1)
}

// Esta función toma una lista de números y devuelve su desviación estándar.
func desviaciónEstándar(números: [Int]) -> Double {
    return sqrt(varianza(números: números))
}

// Esta función toma una lista de números y devuelve su coeficiente de correlación con otra lista de números.
func coeficienteDeCorrelación(x: [Int], y: [Int]) -> Double {
    let mediaX = Double(suma(números: x)) / Double(x.count)
    let mediaY = Double(suma(números: y)) / Double(y.count)
    var covarianza = 0.0
    for i in 0..<x.count {
        covarianza += Double(x[i] - mediaX) * Double(y[i] - mediaY)
    }
    let varianzaX = varianza(números: x)
    let varianzaY = varianza(números: y)
    return covarianza / sqrt(varianzaX * varianzaY)
}

// Esta función toma una lista de números y devuelve su regresión lineal.
func regresiónLineal(x: [Int], y: [Int]) -> (Double, Double) {
    let mediaX = Double(suma(números: x)) / Double(x.count)
    let mediaY = Double(suma(números: y)) / Double(y.count)
    var covarianza = 0.0
    for i in 0..<x.count {
        covarianza += Double(x[i] - mediaX) * Double(y[i] - mediaY)
    }
    let varianzaX = varianza(números: x)
    let pendiente = covarianza / varianzaX
    let ordenadaAlOrigen = mediaY - pendiente * mediaX
    return (pendiente, ordenadaAlOrigen)
}
```

Este código es una colección de funciones estadísticas que se pueden usar para analizar datos. Las funciones incluyen:

* `suma`: Calcula la suma de una lista de números.
* `producto`: Calcula el producto de una lista de números.
* `media`: Calcula la media de una lista de números.
* `mediana`: Calcula la mediana de una lista de números.
* `moda`: Calcula la moda de una lista de números.
* `rango`: Calcula el rango de una lista de números.
* `varianza`: Calcula la varianza de una lista de números.
* `desviaciónEstándar`: Calcula la desviación estándar de una lista de números.
* `coeficienteDeCorrelación`: Calcula el coeficiente de correlación de dos listas de números.
* `regresiónLineal`: Calcula la regresión lineal de dos listas de números.

Estas funciones se pueden usar para analizar datos de muchas maneras diferentes. Por ejemplo, se pueden usar para:

* Encontrar la media, la mediana y la moda de un conjunto de datos.
* Calcular el rango y la