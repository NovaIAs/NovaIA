Claro! Aqui está um código complexo em Groovy para você:

```groovy
import java.util.Random

class JogoAdivinhacao {
    def numeroAleatorio
    def numeroTentativas
    def scanner
    
    JogoAdivinhacao() {
        numeroAleatorio = new Random().nextInt(100) + 1
        numeroTentativas = 0
        scanner = new Scanner(System.in)
    }
    
    void iniciar() {
        println("Bem-vindo ao Jogo de Adivinhação!")
        println("Tente adivinhar o número entre 1 e 100.")
        
        boolean acertou = false
        
        while (!acertou) {
            print("Digite o seu palpite: ")
            def palpite = scanner.nextInt()
            numeroTentativas++
            
            if (palpite < numeroAleatorio) {
                println("Seu palpite foi baixo! Tente novamente.")
            } else if (palpite > numeroAleatorio) {
                println("Seu palpite foi alto! Tente novamente.")
            } else {
                acertou = true
                println("Parabéns! Você acertou o número em ${numeroTentativas} tentativas.")
            }
        }
        
        scanner.close()
        println("O jogo acabou. O número correto era ${numeroAleatorio}.")
    }
}

def jogo = new JogoAdivinhacao()
jogo.iniciar()
```

Este é um jogo de adivinhação em que o jogador precisa adivinhar um número aleatório entre 1 e 100. O código começa definindo uma classe chamada `JogoAdivinhacao`, que possui três variáveis: `numeroAleatorio`, `numeroTentativas` e `scanner`.

No construtor da classe, o código gera um número aleatório entre 1 e 100 usando a classe `Random` da biblioteca Java. Também inicializa o número de tentativas como 0 e cria um objeto `Scanner` para ler a entrada do usuário.

O método `iniciar` é responsável por executar o jogo. Ele exibe uma mensagem de boas-vindas e instruções para o jogador. Em seguida, entra em um loop que continua até que o jogador acerte o número.

Dentro do loop, o jogador é solicitado a digitar um palpite. O número de tentativas é incrementado a cada iteração. Se o palpite for menor que o número aleatório, é exibida uma mensagem informando que o palpite foi baixo. Se o palpite for maior, é exibida uma mensagem informando que o palpite foi alto. Caso contrário, o jogador acertou o número e o loop é encerrado.

Após o loop, o código fecha o objeto `Scanner` e exibe uma mensagem informando o número correto.