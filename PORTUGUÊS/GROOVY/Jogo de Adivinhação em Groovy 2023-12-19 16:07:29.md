Claro! Aqui está um código complexo em Groovy:

```groovy
import java.util.Random

class JogoAdivinhacao {
    def numeroSecreto

    def iniciarJogo() {
        println("Bem-vindo ao Jogo de Adivinhação!")
        println("Estou pensando em um número de 1 a 100. Tente adivinhar!")

        def gerador = new Random()
        numeroSecreto = gerador.nextInt(100) + 1

        def numeroTentativas = 0
        def palpite = 0
        def acertou = false

        while (!acertou) {
            print("Digite seu palpite: ")
            palpite = System.console().readLine() as int
            numeroTentativas++

            if (palpite < numeroSecreto) {
                println("Seu palpite é menor que o número secreto.")
            } else if (palpite > numeroSecreto) {
                println("Seu palpite é maior que o número secreto.")
            } else {
                acertou = true
            }
        }

        println("Parabéns! Você acertou o número secreto em $numeroTentativas tentativas.")
    }
}

def jogo = new JogoAdivinhacao()
jogo.iniciarJogo()
```

Este código implementa um jogo de adivinhação em Groovy. O jogador deve tentar adivinhar um número entre 1 e 100, gerado aleatoriamente. O código utiliza a classe `Random` para gerar o número secreto.

O jogo começa imprimindo uma mensagem de boas-vindas e explicando as regras. Em seguida, o código entra em um loop while, que só será interrompido quando o jogador acertar o número secreto.

Dentro do loop, o código lê o palpite do jogador e compara com o número secreto. Se o palpite for menor ou maior que o número secreto, o código imprime uma mensagem indicando isso. Caso contrário, o jogador acertou e a variável `acertou` é definida como true, encerrando o loop.

Após o jogador acertar, o código imprime uma mensagem de parabéns, informando o número de tentativas que o jogador precisou para acertar.

Por fim, o código cria uma instância da classe `JogoAdivinhacao` e chama o método `iniciarJogo()` para iniciar o jogo.

Espero que este código atenda às suas expectativas! Se tiver alguma dúvida ou precisar de mais ajuda, estou à disposição.