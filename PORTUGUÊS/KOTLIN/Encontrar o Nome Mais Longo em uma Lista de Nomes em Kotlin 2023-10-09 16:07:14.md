Claro! Aqui está um exemplo de código complexo em Kotlin:

```kotlin
fun main() {
    val listaNomes = listOf("João", "Maria", "Pedro", "Ana", "Lucas")
    val nomeMaisLongo = encontrarNomeMaisLongo(listaNomes)
    println("O nome mais longo é: $nomeMaisLongo")
}

fun encontrarNomeMaisLongo(lista: List<String>): String {
    var nomeMaisLongo = ""
    for (nome in lista) {
        if (nome.length > nomeMaisLongo.length) {
            nomeMaisLongo = nome
        }
    }
    return nomeMaisLongo
}
```

Neste código, temos uma função `main()` que serve como ponto de entrada do programa. Dentro dela, definimos uma lista de nomes: "João", "Maria", "Pedro", "Ana" e "Lucas".

Em seguida, chamamos a função `encontrarNomeMaisLongo()`, passando a lista de nomes como argumento. Essa função recebe a lista como parâmetro e retorna o nome mais longo contido nela.

Dentro da função `encontrarNomeMaisLongo()`, inicializamos uma variável `nomeMaisLongo` vazia. Em seguida, percorremos cada nome da lista utilizando um loop `for`. 

Dentro do loop, comparamos o tamanho do nome atual com o tamanho do nome mais longo encontrado até o momento. Se o nome atual for maior, atualizamos o valor da variável `nomeMaisLongo`.

Após percorrer todos os nomes da lista, retornamos o valor da variável `nomeMaisLongo`.

Por fim, na função `main()`, imprimimos o nome mais longo na saída padrão utilizando a função `println()`. O resultado será "O nome mais longo é: Pedro".

Este código demonstra a utilização de estruturas de controle, como loops e condicionais, além de mostrar como trabalhar com listas e manipular strings em Kotlin.