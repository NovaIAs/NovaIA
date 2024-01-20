```kotlin
// CREAR UNA FUNCIÓN QUE RECIBA UNA LISTA DE NÚMEROS Y DEVUELVA EL MAYOR
fun mayor(numeros: List<Int>): Int {
    var mayor = numeros[0] // ESTABLECER EL PRIMER NÚMERO DE LA LISTA COMO EL MAYOR
    for (numero in numeros) { // RECORRER LA LISTA
        if (numero > mayor) { // COMPARAR CADA NÚMERO CON EL MAYOR ACTUAL
            mayor = numero // ACTUALIZAR EL MAYOR SI SE ENCUENTRA UN NÚMERO MAYOR
        }
    }
    return mayor // RETORNAR EL MAYOR
}

// CREAR UNA FUNCIÓN QUE RECIBA UNA LISTA DE NÚMEROS Y DEVUELVA EL MENOR
fun menor(numeros: List<Int>): Int {
    var menor = numeros[0] // ESTABLECER EL PRIMER NÚMERO DE LA LISTA COMO EL MENOR
    for (numero in numeros) { // RECORRER LA LISTA
        if (numero < menor) { // COMPARAR CADA NÚMERO CON EL MENOR ACTUAL
            menor = numero // ACTUALIZAR EL MENOR SI SE ENCUENTRA UN NÚMERO MENOR
        }
    }
    return menor // RETORNAR EL MENOR
}

// CREAR UNA FUNCIÓN QUE RECIBA UNA LISTA DE NÚMEROS Y DEVUELVA EL PROMEDIO
fun promedio(numeros: List<Int>): Double {
    var suma = 0 // SUMAR LOS NÚMEROS DE LA LISTA
    for (numero in numeros) {
        suma += numero
    }
    return suma / numeros.size // DIVIDIR LA SUMA POR LA CANTIDAD DE NÚMEROS PARA OBTENER EL PROMEDIO
}

// CREAR UNA FUNCIÓN QUE RECIBA UNA LISTA DE NÚMEROS Y DEVUELVA LA MEDIANA
fun mediana(numeros: List<Int>): Double {
    val ordenados = numeros.sorted() // ORDENAR LA LISTA DE NÚMEROS
    val mitad = ordenados.size / 2 // CALCULAR LA MITAD DE LA CANTIDAD DE NÚMEROS
    return if (ordenados.size % 2 == 0) { // SI LA CANTIDAD DE NÚMEROS ES PAR
        (ordenados[mitad] + ordenados[mitad - 1]) / 2.0 // RETORNAR EL PROMEDIO DE LOS DOS NÚMEROS DEL MEDIO
    } else { // SI LA CANTIDAD DE NÚMEROS ES IMPAR
        ordenados[mitad] // RETORNAR EL NÚMERO DEL MEDIO
    }
}

// CREAR UNA FUNCIÓN QUE RECIBA UNA LISTA DE NÚMEROS Y DEVUELVA LA MODA
fun moda(numeros: List<Int>): Int {
    val ocurrencias = mutableMapOf<Int, Int>() // CREAR UN MAPA PARA ALMACENAR LAS OCURRENCIAS DE CADA NÚMERO
    for (numero in numeros) { // RECORRER LA LISTA DE NÚMEROS
        ocurrencias[numero] = ocurrencias.getOrDefault(numero, 0) + 1 // AUMENTAR LA OCURRENCIA DEL NÚMERO ACTUAL
    }

    var moda = 0 // INICIALIZAR LA MODA
    var maxOcurrencias = 0 // INICIALIZAR LAS OCURRENCIAS MÁXIMAS

    for ((numero, ocurrencia) in ocurrencias) { // RECORRER EL MAPA DE OCURRENCIAS
        if (ocurrencia > maxOcurrencias) { // SI LAS OCURRENCIAS DEL NÚMERO ACTUAL SON MAYORES QUE LAS OCURRENCIAS MÁXIMAS
            moda = numero // ACTUALIZAR LA MODA
            maxOcurrencias = ocurrencia // ACTUALIZAR LAS OCURRENCIAS MÁXIMAS
        }
    }

    return moda // RETORNAR LA MODA
}

// CREAR UNA FUNCIÓN QUE RECIBA UNA LISTA DE NÚMEROS Y DEVUELVA LA DESVIACIÓN ESTÁNDAR
fun desviacionEstandar(numeros: List<Int>): Double {
    val promedio = promedio(numeros) // CALCULAR EL PROMEDIO DE LA LISTA DE NÚMEROS

    var desviacion = 0.0 // INICIALIZAR LA DESVIACIÓN ESTÁNDAR
    for (numero in numeros) { // RECORRER LA LISTA DE NÚMEROS
        desviacion += Math.pow(numero - promedio, 2.0) // CALCULAR LA DIFERENCIA AL CUADRADO ENTRE EL NÚMERO ACTUAL Y EL PROMEDIO
    }

    desviacion /= (numeros.size - 1) // DIVIDIR LA DESVIACIÓN POR LA CANTIDAD DE NÚMEROS MENOS UNO

    return Math.sqrt(desviacion) // RETORNAR LA RAÍZ CUADRADA DE LA DESVIACIÓN ESTÁNDAR
}

// CREAR UNA FUNCIÓN QUE RECIBA UNA LISTA DE NÚMEROS Y DEVUELVA LA COVARIANZA
fun covarianza(numeros: List<Int>, otrosNumeros: List<Int>): Double {
    if (numeros.size != otrosNumeros.size) { // SI LAS LISTAS DE NÚMEROS TIENEN DIFERENTES TAMAÑOS
        throw IllegalArgumentException("Las listas de números deben tener el mismo tamaño") // LANZAR UNA EXCEPCIÓN
    }

    val promedioNumeros = promedio(numeros) // CALCULAR EL PROMEDIO DE LA LISTA DE NÚMEROS
    val promedioOtrosNumeros = promedio(otrosNumeros) // CALCULAR EL PROMEDIO DE LA LISTA DE OTROS NÚMEROS

    var covarianza = 0.0 // INICIALIZAR LA COVARIANZA
    for (i in numeros.indices) { // RECORRER LOS ÍNDICES DE LAS LISTAS DE NÚMEROS
        covarianza += (numeros[i] - promedioNumeros) * (otrosNumeros[i] - promedioOtrosNumeros) // CALCULAR LA COVARIANZA DE LOS NÚMEROS EN LAS POSICIONES ACTUALES
    }

    covarianza /= (numeros.size - 1) // DIVIDIR LA COVARIANZA POR LA CANTIDAD DE NÚMEROS MENOS UNO

    return covarianza // RETORNAR LA COVARIANZA
}

// CREAR UNA FUNCIÓN QUE RECIBA UNA LISTA DE NÚMEROS Y DEVUELVA LA CORRELACIÓN
fun correlacion(numeros: List<Int>, otrosNumeros: List<Int>): Double {
    if (numeros.size != otrosNumeros.size) { // SI LAS LISTAS DE NÚMEROS TIENEN DIFERENTES TAMAÑOS
        throw IllegalArgumentException("Las listas de números deben tener el mismo tamaño") // LANZAR UNA EXCEPCIÓN
    }

    val covarianza = covarianza(numeros, otrosNumeros) // CALCULAR LA COVARIANZA DE LAS LISTAS DE NÚMEROS

    val desviacionNumeros = desviacionEstandar(numeros) // CALCULAR LA DESVIACIÓN ESTÁNDAR DE LA LISTA DE NÚMEROS
    val desviacionOtrosNumeros = desviacionEstandar(otrosNumeros) // CALCULAR LA DESVIACIÓN ESTÁNDAR DE LA LISTA DE OTROS NÚMEROS

    return covarianza / (desviacionNumeros * desviacionOtrosNumeros) // RETORNAR LA CORRELACIÓN DE LAS LISTAS DE NÚMEROS
}
```

En este ejemplo, hemos creado una serie de funciones que reciben una lista de números y devuelven diferentes medidas estadísticas, como el mayor, el menor, el promedio, la mediana, la moda, la desviación estándar, la covarianza y la correlación. Estas funciones utilizan bucles y otras estructuras de programación avanzadas para calcular y devolver los resultados correctos.