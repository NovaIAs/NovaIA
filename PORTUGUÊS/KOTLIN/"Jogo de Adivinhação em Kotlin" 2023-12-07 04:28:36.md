Claro! Aqui está um exemplo de uma aplicação complexa em Kotlin: um jogo de adivinhação onde o usuário tenta adivinhar um número aleatório.

```kotlin
import java.util.*

fun main() {
    val scanner = Scanner(System.`in`)
    val numeroSecreto = gerarNumeroAleatorio()
    var tentativas = 0
    var acertou = false

    println("Bem-vindo ao jogo de adivinhação!")
    println("Tente adivinhar o número de 1 a 100.")

    while (!acertou) {
        print("Digite um número: ")
        val palpite = scanner.nextInt()
        tentativas++

        when {
            palpite == numeroSecreto -> {
                acertou = true
                println("Parabéns! Você acertou o número em $tentativas tentativas.")
            }
            palpite < numeroSecreto -> println("O número é maior. Tente novamente.")
            palpite > numeroSecreto -> println("O número é menor. Tente novamente.")
        }
    }
}

fun gerarNumeroAleatorio() = (1..100).random()
```

Explicação do código:
1. Importamos a classe `Scanner` para ler a entrada do usuário.
2. A função `main` é o ponto de entrada do programa.
3. Criamos um objeto `Scanner` para ler a entrada do usuário.
4. Geramos um número aleatório entre 1 e 100 usando a função `gerarNumeroAleatorio()`.
5. Inicializamos as variáveis `tentativas` e `acertou`.
6. Mostramos uma mensagem de boas-vindas ao jogador.
7. Iniciamos um loop `while` que vai continuar até o jogador acertar o número.
8. Dentro do loop, pedimos ao jogador um palpite usando o `scanner.nextInt()`.
9. Incrementamos o contador de tentativas.
10. Usamos uma declaração `when` para verificar o palpite do jogador.
    - Se o palpite for igual ao número secreto, definimos `acertou` como `true` e imprimimos uma mensagem de parabéns.
    - Se o palpite for menor que o número secreto, imprimimos uma mensagem dizendo que o número é maior.
    - Se o palpite for maior que o número secreto, imprimimos uma mensagem dizendo que o número é menor.
11. O loop continua até `acertou` se tornar `true`.
12. A função `gerarNumeroAleatorio()` retorna um número aleatório entre 1 e 100 usando a função `random()` do kotlin.

Espero que esse código complexo em Kotlin tenha atendido às suas expectativas! Se você tiver alguma dúvida ou precisar de mais detalhes, fique à vontade para perguntar.