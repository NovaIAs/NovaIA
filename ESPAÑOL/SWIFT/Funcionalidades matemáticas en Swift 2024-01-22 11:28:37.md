```swift
// Definimos una función que calcula el factorial de un número.
func factorial(_ numero: Int) -> Int {
    // Si el número es 0 o 1, devolvemos 1.
    if numero == 0 || numero == 1 {
        return 1
    }
    // Si no, calculamos el factorial del número llamando recursivamente a la función.
    else {
        return numero * factorial(numero - 1)
    }
}

// Definimos una función que calcula el máximo común divisor de dos números.
func maximoComunDivisor(_ numero1: Int, _ numero2: Int) -> Int {
    // Si el primer número es igual a 0, devolvemos el segundo número.
    if numero1 == 0 {
        return numero2
    }
    // Si no, calculamos el máximo común divisor del segundo número y el resto de dividir el primer número entre el segundo.
    else {
        return maximoComunDivisor(numero2 % numero1, numero1)
    }
}

// Definimos una función que calcula el mínimo común múltiplo de dos números.
func minimoComunMultiplo(_ numero1: Int, _ numero2: Int) -> Int {
    // Calculamos el máximo común divisor de los dos números.
    let mcd = maximoComunDivisor(numero1, numero2)
    // Calculamos el mínimo común múltiplo de los dos números dividiendo el producto de los dos números por el máximo común divisor.
    return (numero1 * numero2) / mcd
}

// Definimos una función que calcula la descomposición en factores primos de un número.
func descomposicionEnFactoresPrimos(_ numero: Int) -> [Int] {
    // Creamos una lista vacía para almacenar los factores primos del número.
    var factoresPrimos: [Int] = []
    // Dividimos el número por 2 repetidamente hasta que no sea divisible por 2.
    while numero % 2 == 0 {
        // Añadimos 2 a la lista de factores primos.
        factoresPrimos.append(2)
        // Dividimos el número por 2.
        numero /= 2
    }
    // Recorremos todos los números impares desde 3 hasta la raíz cuadrada del número, incrementando de 2 en 2.
    for i in stride(from: 3, through: Int(sqrt(Double(numero))), by: 2) {
        // Dividimos el número por el número impar actual repetidamente hasta que no sea divisible por el número impar actual.
        while numero % i == 0 {
            // Añadimos el número impar actual a la lista de factores primos.
            factoresPrimos.append(i)
            // Dividimos el número por el número impar actual.
            numero /= i
        }
    }
    // Si el número no es 1, añadimos el número a la lista de factores primos.
    if numero > 1 {
        factoresPrimos.append(numero)
    }
    // Devolvemos la lista de factores primos del número.
    return factoresPrimos
}

// Definimos una función que calcula la suma de los dígitos de un número.
func sumaDeDigitos(_ numero: Int) -> Int {
    // Convertimos el número a una cadena de caracteres.
    let cadena = String(numero)
    // Creamos una variable para almacenar la suma de los dígitos del número.
    var sumaDeDigitos = 0
    // Recorremos todos los caracteres de la cadena de caracteres.
    for caracter in cadena {
        // Convertimos el carácter actual a un número entero.
        let digito = Int(String(caracter))!
        // Añadimos el número entero actual a la suma de los dígitos del número.
        sumaDeDigitos += digito
    }
    // Devolvemos la suma de los dígitos del número.
    return sumaDeDigitos
}

// Definimos una función que calcula el número de divisores de un número.
func numeroDeDivisores(_ numero: Int) -> Int {
    // Calculamos la descomposición en factores primos del número.
    let factoresPrimos = descomposicionEnFactoresPrimos(numero)
    // Creamos un diccionario para almacenar los exponentes de los factores primos del número.
    var exponentes: [Int: Int] = [:]
    // Recorremos todos los factores primos del número.
    for factorPrimo in factoresPrimos {
        // Si el factor primo no está en el diccionario, lo añadimos con un exponente de 1.
        if exponentes[factorPrimo] == nil {
            exponentes[factorPrimo] = 1
        }
        // Si el factor primo ya está en el diccionario, incrementamos su exponente.
        else {
            exponentes[factorPrimo]! += 1
        }