Aqui está um exemplo de um código complexo em Kotlin que utiliza algumas funcionalidades avançadas da linguagem:

```kotlin
import kotlin.math.pow

fun main() {
    val numeros = listOf(1, 2, 3, 4, 5)
    
    val resultado = numeros
        .filter { it % 2 == 0 }
        .map { it.toDouble() }
        .map { it.pow(2) }
        .reduce { acc, valor -> acc + valor }
    
    println("O resultado é: $resultado")
}
```

Neste código, temos um exemplo de operações de filtragem, mapeamento e redução aplicadas a uma lista de números. 

Primeiramente, criamos uma lista de números utilizando a função `listOf`. Em seguida, utilizamos a função `filter` para filtrar apenas os números pares. 

Depois, utilizamos a função `map` duas vezes. A primeira vez, para converter os números inteiros em números de ponto flutuante (Double). Em seguida, elevamos cada número ao quadrado utilizando a função `pow` da classe `Double`.

Por fim, utilizamos a função `reduce` para somar todos os números resultantes e armazenamos o resultado na variável `resultado`. Por fim, imprimimos o resultado na tela utilizando a função `println`.

Este código demonstra algumas das funcionalidades avançadas e expressivas da linguagem Kotlin, como as funções de ordem superior (`filter`, `map`, `reduce`) e o uso de lambdas para definir as operações a serem realizadas em cada elemento da lista.