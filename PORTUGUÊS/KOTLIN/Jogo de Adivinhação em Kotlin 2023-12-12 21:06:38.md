Certamente! Aqui está um exemplo de código complexo em Kotlin que envolve várias funcionalidades e conceitos avançados:

```kotlin
import java.util.*

fun main() {
    val scanner = Scanner(System.`in`)
    println("Bem-vindo ao jogo de adivinhação!")

    val numSecreto = gerarNumeroSecreto()
    var numTentativas = 1
    var acertou = false

    while (!acertou) {
        print("Digite um número entre 1 e 100: ")
        val numGuess = scanner.nextInt()
        val resultado = verificarPalpite(numGuess, numSecreto)

        when (resultado) {
            0 -> {
                acertou = true
                println("Parabéns! Você acertou o número secreto em $numTentativas tentativa(s)!")
            }
            1 -> println("O número secreto é menor que $numGuess. Tente novamente!")
            2 -> println("O número secreto é maior que $numGuess. Tente novamente!")
        }

        numTentativas++
    }
}

fun gerarNumeroSecreto(): Int {
    return Random().nextInt(100) + 1
}

fun verificarPalpite(palpite: Int, numSecreto: Int): Int {
    return when {
        palpite == numSecreto -> 0
        palpite < numSecreto -> 1
        else -> 2
    }
}
```

Este código implementa um jogo de adivinhação em Kotlin. Aqui está uma explicação passo a passo de como funciona:

- Primeiro, importamos a classe `Scanner` para ler as entradas do usuário.

- Em seguida, exibimos uma mensagem de boas-vindas ao usuário.

- A função `gerarNumeroSecreto` é chamada para gerar um número aleatório entre 1 e 100, que será o número a ser adivinhado pelo usuário.

- Definimos as variáveis `numTentativas` e `acertou` para acompanhar o número de tentativas do usuário e se ele acertou ou não.

- Dentro do loop `while`, solicitamos ao usuário que digite um número e lemos a entrada usando o `Scanner`.

- A função `verificarPalpite` é chamada para verificar se o palpite do usuário é igual, menor ou maior que o número secreto.

- Dependendo do resultado, uma mensagem apropriada é exibida.

- Se o palpite for igual ao número secreto, a variável `acertou` é alterada para `true` e exibimos uma mensagem de parabéns com o número de tentativas.

- Caso contrário, incrementamos o número de tentativas e continuamos o loop.

- A função `gerarNumeroSecreto` usa a classe `Random` para gerar um número aleatório entre 1 e 100.

- A função `verificarPalpite` compara o palpite do usuário com o número secreto e retorna um valor diferente dependendo do resultado.

Espero que este código complexo atenda às suas expectativas!