Claro! Aqui está um código complexo em Groovy:

```
import java.util.Random

class JogoAdivinhacao {
    def numerosSorteados = []
    def tentativas = 0
    def maxTentativas = 10
    
    JogoAdivinhacao() {
        def random = new Random()
        for (int i = 0; i < 100; i++) {
            numerosSorteados << random.nextInt(1000)
        }
    }
    
    def jogar() {
        println("Bem-vindo ao Jogo de Adivinhação!")
        println("Tente adivinhar o número sorteado (entre 0 e 1000) em até $maxTentativas tentativas.")
        
        while (tentativas < maxTentativas) {
            def numeroEscolhido = lerNumero()
            tentativas++
            
            if (verificarAcerto(numeroEscolhido)) {
                println("Parabéns! Você acertou em $tentativas tentativas.")
                return
            } else {
                println("Você errou! Tente novamente.")
            }
        }
        
        println("Suas $maxTentativas tentativas acabaram. O número sorteado era ${numerosSorteados[0]}. Tente novamente!")
    }
    
    def lerNumero() {
        print("Digite um número: ")
        return System.console().readLine().toInteger()
    }
    
    def verificarAcerto(numero) {
        return numerosSorteados.contains(numero)
    }
}

def jogo = new JogoAdivinhacao()
jogo.jogar()
```

Este código em Groovy implementa um jogo de adivinhação. O jogo consiste em tentar adivinhar um número sorteado dentro de um intervalo de 0 a 1000. O jogador tem um número máximo de tentativas (definido como 10 no código, mas pode ser alterado) para acertar o número.

No início, o código gera uma lista de 100 números aleatórios entre 0 e 1000. Em seguida, o método `jogar()` é chamado, que imprime instruções e inicia um loop enquanto o número de tentativas é menor que o número máximo de tentativas.

Dentro do loop, o método `lerNumero()` é usado para obter o número fornecido pelo jogador. Esse número é comparado com a lista de números sorteados usando o método `verificarAcerto()`. Se o número fornecido estiver na lista, o jogador acertou e uma mensagem de parabéns é impressa. Caso contrário, o jogador errou e uma mensagem de erro é impressa.

Se o número máximo de tentativas for alcançado sem que o jogador acerte, uma mensagem contendo o número correto é impressa.

Para jogar o jogo, basta executar o código em um ambiente que suporte Groovy. O jogo irá solicitar ao jogador que digite um número, e o jogador deve inserir um valor inteiro entre 0 e 1000. O código então fornecerá feedback ao jogador, informando se o número foi acertado ou não, e assim por diante.

Espero que este código atenda às suas expectativas de complexidade e que você possa aprender com ele!