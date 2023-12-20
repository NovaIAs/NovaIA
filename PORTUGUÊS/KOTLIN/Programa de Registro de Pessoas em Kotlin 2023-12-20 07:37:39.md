Claro! Aqui está um exemplo de um código complexo em Kotlin:

```kotlin
import java.util.*

fun main() {
    val scanner = Scanner(System.`in`)
    val listaNomes = mutableListOf<String>()
    val listaIdades = mutableListOf<Int>()

    println("Bem-vindo(a)! Este programa irá pedir que você insira o nome e a idade de várias pessoas.")

    while (true) {
        println("Insira o nome da pessoa ou 'sair' para encerrar o programa:")
        val nome = scanner.nextLine()

        if (nome.toLowerCase() == "sair") {
            break
        }

        println("Insira a idade de $nome:")
        val idade = scanner.nextInt()
        scanner.nextLine()

        listaNomes.add(nome)
        listaIdades.add(idade)
    }

    println("\nDados das pessoas registradas:")
    for (i in listaNomes.indices) {
        println("${listaNomes[i]} - ${listaIdades[i]} anos")
    }

    val mediaIdades = listaIdades.average()
    val pessoaMaisVelha = listaNomes[listaIdades.indexOf(listaIdades.maxOrNull())]
    val pessoaMaisNova = listaNomes[listaIdades.indexOf(listaIdades.minOrNull())]

    println("\nEstatísticas:")
    println("Média de idades: ${String.format("%.2f", mediaIdades)} anos")
    println("Pessoa mais velha: $pessoaMaisVelha")
    println("Pessoa mais nova: $pessoaMaisNova")
}
```

Este código é um exemplo de um programa que solicita ao usuário que insira o nome e a idade de várias pessoas. Ele armazena essas informações em duas listas: `listaNomes` e `listaIdades`. O programa continua solicitando os dados até que o usuário digite "sair".

Após a coleta dos dados, o programa exibe os nomes e as idades das pessoas registradas. Em seguida, calcula a média das idades, encontra a pessoa mais velha e a pessoa mais nova com base nas idades registradas.

O código utiliza a estrutura de repetição `while (true)` para continuar solicitando dados até que o usuário decida sair. Também utiliza funções como `average()` para calcular a média das idades e `maxOrNull()` e `minOrNull()` para encontrar a pessoa mais velha e a pessoa mais nova, respectivamente.

Por fim, as estatísticas são exibidas na saída do programa.

Espero que este exemplo atenda às suas expectativas!