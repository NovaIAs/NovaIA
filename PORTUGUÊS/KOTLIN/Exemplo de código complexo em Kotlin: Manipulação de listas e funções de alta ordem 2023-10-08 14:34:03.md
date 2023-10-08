Claro! Aqui está um exemplo de código complexo em Kotlin:

```kotlin
// Classe principal
class Main {
    // Função principal
    fun main() {
        // Inicializa uma lista de números
        val numeros = mutableListOf<Int>()

        // Preenche a lista com números aleatórios
        for (i in 1..10) {
            numeros.add((1..100).random())
        }

        // Filtra os números pares
        val numerosPares = numeros.filter { it % 2 == 0 }

        // Ordena os números pares em ordem decrescente
        numerosPares.sortDescending()

        // Imprime os números pares ordenados
        println("Números pares ordenados: $numerosPares")

        // Calcula a média dos números pares
        val media = numerosPares.average()

        // Imprime a média
        println("Média dos números pares: $media")

        // Verifica se algum número par é maior que 50
        val existeMaiorQue50 = numerosPares.any { it > 50 }

        // Imprime o resultado
        if (existeMaiorQue50) {
            println("Existem números pares maiores que 50")
        } else {
            println("Não existem números pares maiores que 50")
        }
    }
}
```

Neste código em Kotlin, criamos uma classe `Main` com uma função `main`, que é a função de entrada do programa. 

Dentro da função `main`, inicializamos uma lista chamada `numeros` e preenchemos ela com 10 números aleatórios entre 1 e 100. 

Em seguida, utilizamos a função `filter` para filtrar apenas os números pares da lista `numeros` e armazenamos o resultado na lista `numerosPares`. 

Após isso, utilizamos a função `sortDescending` para ordenar os números pares em ordem decrescente. 

Em seguida, imprimimos os números pares ordenados. 

Calculamos a média dos números pares utilizando a função `average` e imprimimos o resultado.

Por fim, utilizamos a função `any` para verificar se existe algum número par maior que 50 e imprimimos o resultado correspondente. 

Este código demonstra o uso de funções de alta ordem (como `filter`, `sortDescending`, `average`, `any`) e manipulação de listas em Kotlin.