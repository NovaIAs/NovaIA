Certamente! Aqui está um exemplo de um código complexo em Kotlin que envolve manipulação de listas e cálculos matemáticos:

```kotlin
fun main() {
    val numeros = mutableListOf<Int>()
    val pares = mutableListOf<Int>()
    val impares = mutableListOf<Int>()

    // Preenche a lista 'numeros' com números aleatórios entre 1 e 100
    for (i in 1..100) {
        numeros.add((1..100).random())
    }

    // Separa os números pares e ímpares em listas diferentes
    for (numero in numeros) {
        if (numero % 2 == 0) {
            pares.add(numero)
        } else {
            impares.add(numero)
        }
    }

    // Calcula a média dos números pares
    val mediaPares = pares.average()

    // Calcula o desvio padrão dos números ímpares
    val desvioPadraoImpares = calcularDesvioPadrao(impares)

    println("Números: $numeros")
    println("Pares: $pares")
    println("Ímpares: $impares")
    println("Média dos números pares: $mediaPares")
    println("Desvio padrão dos números ímpares: $desvioPadraoImpares")
}

fun calcularDesvioPadrao(numeros: List<Int>): Double {
    val media = numeros.average()
    val somaQuadrados = numeros.map { (it - media) * (it - media) }.sum()
    val variancia = somaQuadrados / numeros.size
    return Math.sqrt(variancia)
}
```

Neste código, primeiro criamos três listas vazias: `numeros`, `pares` e `impares`. Em seguida, usamos um loop para preencher a lista `numeros` com 100 números aleatórios entre 1 e 100.

Depois disso, percorremos a lista `numeros` e se um número for par, adicionamos à lista `pares`, caso contrário, adicionamos à lista `impares`.

Em seguida, calculamos a média dos números pares usando a função `average()` e o desvio padrão dos números ímpares usando um helper function chamado `calcularDesvioPadrao()`. Nessa função, calculamos a média, a soma dos quadrados das diferenças entre cada número e a média, a variância e, finalmente, retornamos a raiz quadrada da variância usando a função `Math.sqrt()`.

Por fim, imprimimos as listas `numeros`, `pares` e `impares`, bem como a média dos números pares e o desvio padrão dos números ímpares.