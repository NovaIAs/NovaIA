Claro! Aqui está um exemplo de um código complexo em Kotlin:

```kotlin
fun main() {
    val listaNumeros = mutableListOf<Int>()
    var somaPares = 0
    var produtoImpares = 1

    for (i in 1..100) {
        listaNumeros.add(i)
    }

    for (numero in listaNumeros) {
        if (numero % 2 == 0) {
            somaPares += numero
        } else {
            produtoImpares *= numero
        }
    }

    println("A soma dos números pares é: $somaPares")
    println("O produto dos números ímpares é: $produtoImpares")
}
```

Neste código em Kotlin, temos uma função principal `main()` que realiza o seguinte:

1. Cria uma lista `listaNumeros` vazia do tipo `MutableList<Int>`.
2. Declara as variáveis `somaPares` e `produtoImpares` e as inicializa com os valores 0 e 1, respectivamente.
3. Utiliza um loop `for` para adicionar números de 1 a 100 na lista `listaNumeros`.
4. Em seguida, percorre cada número na lista `listaNumeros` utilizando outro loop `for`.
5. Verifica se o número é par ou ímpar utilizando o operador `%` (resto da divisão por 2).
6. Se o número for par, adiciona-o à variável `somaPares`.
7. Caso contrário, multiplica-o à variável `produtoImpares`.
8. No final, imprime na tela a soma dos números pares e o produto dos números ímpares.

Esse código demonstra algumas funcionalidades do Kotlin, como a declaração de variáveis, loops, listas e operadores condicionais. Ele calcula a soma dos números pares de 1 a 100 e o produto dos números ímpares, exibindo os resultados na tela.