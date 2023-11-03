Claro! Aqui está um código complexo em Kotlin para você. É um programa que implementa um jogo de adivinhação onde o jogador precisa adivinhar um número dentro de um intervalo específico.

```kotlin
import java.util.*

fun main() {
    val scanner = Scanner(System.`in`)

    println("Bem-vindo ao jogo de adivinhação!")
    println("Digite o valor mínimo do intervalo:")
    val min = scanner.nextInt()
    
    println("Digite o valor máximo do intervalo:")
    val max = scanner.nextInt()
    
    val numeroSecreto = gerarNumeroSecreto(min, max)
    var tentativas = 0
    var acertou = false
    
    while (!acertou) {
        println("Digite um número:")
        val palpite = scanner.nextInt()
        tentativas++
        
        if (palpite == numeroSecreto) {
            acertou = true
            println("Parabéns, você acertou o número em $tentativas tentativas!")
        } else if (palpite < numeroSecreto) {
            println("Chute um número maior!")
        } else {
            println("Chute um número menor!")
        }
    }
}

fun gerarNumeroSecreto(min: Int, max: Int): Int {
    return Random().nextInt(max - min + 1) + min
}
```

Nesse código, começamos importando a classe `Scanner` para leitura de entrada do usuário. Em seguida, temos a função `main` que é o ponto de entrada do programa.

No início do programa, pedimos ao jogador para digitar o valor mínimo e máximo do intervalo em que ele deseja adivinhar o número. Esses valores são armazenados nas variáveis `min` e `max`, respectivamente.

Em seguida, chamamos a função `gerarNumeroSecreto` para obter um número aleatório dentro do intervalo especificado. Essa função utiliza a classe `Random` para gerar um número inteiro entre `min` e `max`, inclusivos.

Dentro do loop `while`, o jogador é solicitado a digitar um número e o palpite é armazenado na variável `palpite`. A cada tentativa, incrementamos a variável `tentativas`.

Se o palpite for igual ao número secreto, o jogador acertou e o loop é interrompido. Caso contrário, são exibidas dicas para o jogador ajustar seu palpite: se o palpite for menor que o número secreto, é exibida a mensagem "Chute um número maior!" e vice-versa.

Após sair do loop, exibimos uma mensagem de parabéns com o número de tentativas realizadas pelo jogador.

Espero que este código atenda às suas expectativas!