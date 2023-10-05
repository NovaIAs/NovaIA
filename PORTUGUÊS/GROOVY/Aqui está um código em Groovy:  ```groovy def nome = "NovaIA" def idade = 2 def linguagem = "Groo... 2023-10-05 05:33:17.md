Claro! Aqui está um código complexo em Groovy:

```groovy
import java.util.Random

class JogoAdivinhacao {
    def iniciarJogo() {
        def numeroAleatorio = gerarNumeroAleatorio()
        def tentativas = 0
        def numeroEscolhido

        println("Bem-vindo ao Jogo de Adivinhação!")
        println("Tente adivinhar o número entre 1 e 100.")

        while (numeroEscolhido != numeroAleatorio) {
            tentativas++
            numeroEscolhido = obterNumeroDoUsuario()

            if (numeroEscolhido > numeroAleatorio) {
                println("Tente um número menor.")
            } else if (numeroEscolhido < numeroAleatorio) {
                println("Tente um número maior.")
            }
        }

        println("Parabéns! Você acertou o número em $tentativas tentativas.")
    }

    def gerarNumeroAleatorio() {
        def random = new Random()
        return random.nextInt(100) + 1
    }

    def obterNumeroDoUsuario() {
        print("Digite um número: ")
        return System.console().readLine() as int
    }
}

def jogo = new JogoAdivinhacao()
jogo.iniciarJogo()
```

Este código implementa um jogo de adivinhação simples em Groovy. O jogador deve tentar adivinhar um número gerado aleatoriamente entre 1 e 100. O jogo começa pedindo ao jogador para digitar um número. Em seguida, o número é comparado com o número gerado aleatoriamente e são exibidas dicas para o jogador, caso o número escolhido seja maior ou menor. O jogo continua até que o jogador adivinhe corretamente o número.

O código utiliza uma classe `JogoAdivinhacao` para encapsular a lógica do jogo. A função `iniciarJogo()` é responsável por iniciar o jogo, gerar o número aleatório, solicitar o número do jogador e fornecer feedback. A função `gerarNumeroAleatorio()` utiliza a classe `Random` para gerar um número aleatório entre 1 e 100. A função `obterNumeroDoUsuario()` solicita ao jogador que digite um número utilizando a função `System.console().readLine()`.

Por fim, é criada uma instância da classe `JogoAdivinhacao` e o jogo é iniciado chamando o método `iniciarJogo()`.