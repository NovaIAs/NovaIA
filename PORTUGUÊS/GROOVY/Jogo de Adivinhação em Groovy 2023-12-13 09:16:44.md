Claro! Aqui está um código complexo em Groovy como você solicitou:

```groovy
import java.util.Random

class JogoAdivinhacao {
    def numeroSecreto
    def contadorTentativas

    JogoAdivinhacao() {
        Random gerador = new Random()
        numeroSecreto = gerador.nextInt(100) + 1
        contadorTentativas = 0
    }

    void iniciar() {
        println "Bem-vindo ao Jogo da Adivinhação!"
        println "Tente adivinhar o número entre 1 e 100.\n"

        def continuar = true
        while (continuar) {
            def palpite = receberPalpite()

            if (verificarPalpite(palpite)) {
                println "Parabéns! Você acertou o número em ${contadorTentativas} tentativas."
                continuar = false
            } else {
                println "Tente novamente.\n"
            }
        }

        println "\nObrigado por jogar!"
    }

    int receberPalpite() {
        def entradaValida = false
        def palpite = 0

        while (!entradaValida) {
            try {
                print "Digite seu palpite: "
                palpite = Integer.parseInt(System.console().readLine())
                entradaValida = true
            } catch (NumberFormatException e) {
                println "Entrada inválida. Por favor, digite um número inteiro.\n"
            }
        }

        contadorTentativas++
        return palpite
    }

    boolean verificarPalpite(int palpite) {
        if (palpite < numeroSecreto) {
            println "O número secreto é maior."
            return false
        } else if (palpite > numeroSecreto) {
            println "O número secreto é menor."
            return false
        } else {
            return true
        }
    }
}

def jogo = new JogoAdivinhacao()
jogo.iniciar()
```

Neste código, criei uma classe `JogoAdivinhacao`, que implementa um jogo de adivinhação de um número secreto entre 1 e 100. O jogador deve tentar adivinhar o número em quantas tentativas forem necessárias.

O código começa importando a classe `Random` da biblioteca padrão do Java, que é utilizada para gerar números aleatórios. Em seguida, a classe `JogoAdivinhacao` é definida.

Dentro da classe, temos duas variáveis de instância: `numeroSecreto` e `contadorTentativas`. A primeira armazena o número que o jogador deve adivinhar, gerado aleatoriamente no construtor da classe. A segunda armazena o número de tentativas feitas pelo jogador.

O método `iniciar()` é o ponto de entrada do jogo. Ele exibe uma mensagem de boas-vindas e inicia um loop que continua até o jogador acertar o número secreto. Dentro do loop, o método `receberPalpite()` é chamado para obter o palpite do jogador. Esse método verifica se a entrada é um número inteiro válido antes de retornar o palpite.

Após receber o palpite, o método `verificarPalpite()` é chamado para comparar o palpite com o número secreto. Se o palpite for menor ou maior, uma mensagem é exibida para indicar isso e o loop continua. Se o palpite for igual ao número secreto, uma mensagem de parabéns é exibida, juntamente com o número de tentativas feitas, e o loop é encerrado.

Por fim, é exibida uma mensagem de agradecimento por jogar.

Espero que este código atenda às suas expectativas!