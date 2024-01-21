```swift
// Función que recibe una lista de números y devuelve la suma de los mismos.

func sumaDeLista(lista: [Int]) -> Int {
    var suma = 0 // Variable para almacenar la suma de los números.

    // Recorremos la lista de números.

    for numero in lista {
        // Sumamos cada número a la variable "suma".

        suma += numero
    }

    // Devolvemos la suma de los números.

    return suma
}

// Función que recibe una lista de números y devuelve el promedio de los mismos.

func promedioDeLista(lista: [Int]) -> Float {
    // Calculamos la suma de los números utilizando la función "sumaDeLista".

    let suma = sumaDeLista(lista: lista)

    // Calculamos el promedio de los números dividiendo la suma por el número de elementos de la lista.

    let promedio = Float(suma) / Float(lista.count)

    // Devolvemos el promedio de los números.

    return promedio
}

// Función que recibe una lista de números y devuelve el número máximo de la lista.

func maximoDeLista(lista: [Int]) -> Int {
    // Variable para almacenar el número máximo.

    var maximo = lista[0] // Inicializamos la variable con el primer elemento de la lista.

    // Recorremos la lista de números.

    for numero in lista {
        // Si el número actual es mayor que el número máximo, actualizamos la variable "maximo".

        if numero > maximo {
            maximo = numero
        }
    }

    // Devolvemos el número máximo.

    return maximo
}

// Función que recibe una lista de números y devuelve el número mínimo de la lista.

func minimoDeLista(lista: [Int]) -> Int {
    // Variable para almacenar el número mínimo.

    var minimo = lista[0] // Inicializamos la variable con el primer elemento de la lista.

    // Recorremos la lista de números.

    for numero in lista {
        // Si el número actual es menor que el número mínimo, actualizamos la variable "minimo".

        if numero < minimo {
            minimo = numero
        }
    }

    // Devolvemos el número mínimo.

    return minimo
}

// Función que recibe una lista de números y devuelve la mediana de la lista.

func medianaDeLista(lista: [Int]) -> Float {
    // Ordenamos la lista de números en orden ascendente.

    let listaOrdenada = lista.sorted()

    // Calculamos el índice del elemento central de la lista.

    let indiceCentral = listaOrdenada.count / 2

    // Si el número de elementos de la lista es impar, devolvemos el elemento central.

    if listaOrdenada.count % 2 == 1 {
        return Float(listaOrdenada[indiceCentral])
    }
    // Si el número de elementos de la lista es par, devolvemos la media de los dos elementos centrales.

    else {
        let elementoCentral1 = listaOrdenada[indiceCentral - 1]
        let elementoCentral2 = listaOrdenada[indiceCentral]
        return Float(elementoCentral1 + elementoCentral2) / 2.0
    }
}

// Función que recibe una lista de números y devuelve la desviación estándar de la lista.

func desviacionEstandarDeLista(lista: [Int]) -> Float {
    // Calculamos la media de la lista utilizando la función "promedioDeLista".

    let media = promedioDeLista(lista: lista)

    // Variable para almacenar la suma de los cuadrados de las diferencias entre cada número y la media.

    var sumaCuadradosDiferencias = 0.0

    // Recorremos la lista de números.

    for numero in lista {
        // Calculamos la diferencia entre el número actual y la media.

        let diferencia = numero - media

        // Calculamos el cuadrado de la diferencia.

        let cuadradoDiferencia = diferencia * diferencia

        // Sumamos el cuadrado de la diferencia a la variable "sumaCuadradosDiferencias".

        sumaCuadradosDiferencias += cuadradoDiferencia
    }

    // Calculamos la varianza dividiendo la suma de los cuadrados de las diferencias entre el número de elementos de la lista menos uno.

    let varianza = sumaCuadradosDiferencias / Float(lista.count - 1)

    // Calculamos la desviación estándar tomando la raíz cuadrada de la varianza.

    let desviacionEstandar = sqrt(varianza)

    // Devolvemos la desviación estándar.

    return desviacionEstandar
}

// Función que recibe una lista de números y devuelve el coeficiente de correlación entre la lista y otra lista.

func coeficienteDeCorrelacion(lista1: [Int], lista2: [Int]) -> Float {
    // Calculamos la media de la primera lista utilizando la función "promedioDeLista".

    let mediaLista1 = promedioDeLista(lista: lista1)

    // Calculamos la media de la segunda lista utilizando la función "promedioDeLista".

    let mediaLista2 = promedioDeLista(lista: lista2)

    // Variables para almacenar la suma de los productos de las diferencias entre cada número y la media de las dos listas.

    var sumaProductosDiferencias = 0.0

    // Recorremos la lista de números.

    for i in 0..<lista1.count {
        // Calculamos la diferencia entre el número actual de la primera lista y la media de la primera lista.

        let diferenciaLista1 = lista1[i] - mediaLista1

        // Calculamos la diferencia entre el número actual de la segunda lista y la media de la segunda lista.

        let diferenciaLista2 = lista2[i] - mediaLista2

        // Calculamos el producto de las diferencias.

        let productoDiferencias = diferenciaLista1 * diferenciaLista2

        // Sumamos el producto de las diferencias a la variable "sumaProductosDiferencias".

        sumaProductosDiferencias += productoDiferencias
    }

    // Calculamos la varianza de la primera lista utilizando la función "desviacionEstandarDeLista".

    let varianzaLista1 = desviacionEstandarDeLista(lista: lista1)

    // Calculamos la varianza de la segunda lista utilizando la función "desviacionEstandarDeLista".

    let varianzaLista2 = desviacionEstandarDeLista(lista: lista2)

    // Calculamos el coeficiente de correlación dividiendo la suma de los productos de las diferencias entre el producto de las desviaciones estándar de las dos listas.

    let coeficienteDeCorrelacion = sumaProductosDiferencias / (varianzaLista1 * varianzaLista2)

    // Devolvemos el coeficiente de correlación.

    return coeficienteDeCorrelacion
}

// Ejemplo de uso de las funciones.

let lista1 = [1, 2, 3, 4, 5]
let lista2 = [6, 7, 8, 9, 10]

// Calculamos la suma de la lista 1 utilizando la función "sumaDeLista".

let sumaLista1 = sumaDeLista(lista: lista1)

// Calculamos el promedio de la lista 1 utilizando la función "promedioDeLista".

let promedioLista1 = promedioDeLista(lista: lista1)

// Calculamos el número máximo de la lista 1 utilizando la función "maximoDeLista".

let maximoLista1 = maximoDeLista(lista: lista1)

// Calculamos el número mínimo de la lista 1 utilizando la función "minimoDeLista".

let minimoLista1 = minimoDeLista(lista: lista1)

// Calculamos la mediana de la lista 1 utilizando la función "medianaDeLista".

let medianaLista1 = medianaDeLista(lista: lista1)

// Calculamos la desviación estándar de la lista 1 utilizando la función "desviacionEstandarDeLista".

let desviacionEstandarLista1 = desviacionEstandarDeLista(lista: lista1)

// Calculamos el coeficiente de correlación entre la lista 1 y la lista 2 utilizando la función "coeficienteDeCorrelacion".

let coeficienteDeCorrelacionLista1Lista2 = coeficienteDeCorrelacion(lista1: lista1, lista2: lista2)

// Imprimimos los resultados.

print("Suma de la lista 1:", sumaLista1)
print("Promedio de la lista 1:", promedioLista1)
print("Número máximo de la lista 1:", maximoLista1)
print("Número mínimo de la lista 1:", minimoLista1)
print("Mediana de la lista 1:", medianaLista1)
print("Desviación estándar de la lista 1:", desviacionEstandarLista1)
print("Coeficiente de correlación entre la lista 1 y la lista 2:", coeficienteDeCorrelacionLista1Lista2)
```

**Explicación del código:**

El código anterior consta de una serie de funciones que realizan diferentes operaciones matemáticas sobre una lista de números.

La primera función, `sumaDeLista`, recibe una lista de números y devuelve la suma de los mismos.

La segunda función, `promedioDeLista`, recibe una lista de números y devuelve el promedio de los mismos.

La tercera función, `maximoDeLista`, recibe una lista de números y devuelve el número máximo de la lista.

La cuarta función, `minimoDeLista`, recibe una lista de números y devuelve el número mínimo de la lista.

La quinta función, `medianaDeLista`, recibe una lista de números y devuelve la mediana de la lista.

La sexta función, `desviacionEstandarDeLista`, recibe una lista de números y devuelve la desviación estándar de la lista.

La séptima función, `coeficienteDeCorrelacion`, recibe dos listas de números y devuelve el coeficiente de correlación entre las dos listas.

El código también incluye un ejemplo de uso de las funciones anteriores, que calcula la suma, el promedio, el número máximo, el número mínimo, la mediana, la desviación estándar y el coeficiente de correlación de una lista de números.