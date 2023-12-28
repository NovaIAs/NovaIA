Certamente! Aqui está um exemplo de código complexo em Kotlin:

```kotlin
fun main() {
    val numeros = mutableListOf<Int>()

    for (i in 1..100) {
        numeros.add(i)
    }

    val somaPares = numeros.filter { it % 2 == 0 }.sum()
    val produtoImpares = numeros.filter { it % 2 != 0 }.reduce { acc, i -> acc * i }

    val resultado = if (somaPares > produtoImpares) {
        "A soma dos números pares é maior que o produto dos números ímpares."
    } else if (somaPares < produtoImpares) {
        "O produto dos números ímpares é maior que a soma dos números pares."
    } else {
        "A soma dos números pares é igual ao produto dos números ímpares."
    }

    println(resultado)
}
```

Neste exemplo, primeiro criamos uma lista chamada `numeros` e a preenchemos com os números de 1 a 100 utilizando um loop `for`. 

Em seguida, utilizamos a função `filter` para filtrar apenas os números pares e ímpares da lista `numeros`. Utilizamos a função `sum()` para obter a soma dos números pares e a função `reduce()` para obter o produto dos números ímpares.

Depois, comparamos a soma dos números pares com o produto dos números ímpares utilizando uma estrutura `if-else`. Dependendo do resultado da comparação, armazenamos uma mensagem apropriada na variável `resultado`.

Por fim, imprimimos o conteúdo da variável `resultado`.

Espero que este código complexo em Kotlin atenda às suas expectativas!