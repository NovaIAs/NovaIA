Claro! Aqui está um exemplo de código em Kotlin que cria uma lista de números primos até um determinado limite:

```kotlin
fun main() {
    val limite = 100
    val numerosPrimos = mutableListOf<Int>()

    for (numero in 2..limite) {
        var ehPrimo = true

        for (divisor in 2 until numero) {
            if (numero % divisor == 0) {
                ehPrimo = false
                break
            }
        }

        if (ehPrimo) {
            numerosPrimos.add(numero)
        }
    }

    println("Números primos até $limite: ")
    numerosPrimos.forEach { println(it) }
}
```

Explicação do código:

1. A função `main` é o ponto de entrada do programa.
2. A variável `limite` define até qual número queremos encontrar os números primos.
3. A variável `numerosPrimos` é uma lista mutável que irá armazenar os números primos encontrados.
4. O primeiro loop `for` percorre todos os números de 2 até o `limite`.
5. A variável `ehPrimo` é inicializada como `true` para cada número.
6. O segundo loop `for` percorre todos os possíveis divisores do número atual.
7. Se o número atual for divisível por algum divisor, então ele não é primo e a variável `ehPrimo` é alterada para `false`.
8. Se o número atual for primo (ou seja, `ehPrimo` não foi alterada para `false`), ele é adicionado à lista `numerosPrimos`.
9. Após a execução dos loops, os números primos encontrados são impressos na tela.

Esse código utiliza dois loops aninhados para verificar se um número é primo. Ele testa se o número atual é divisível por qualquer número entre 2 e ele mesmo menos 1. Se o número for divisível por algum desses valores, ele não é primo. Caso contrário, ele é considerado primo e adicionado à lista de números primos encontrados.