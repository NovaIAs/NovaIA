Claro! Aqui está um código em Swift que implementa um jogo de adivinhação com várias funcionalidades adicionais, como pontuação, níveis de dificuldade e dicas. Vou dar uma breve explicação de cada parte do código abaixo:

```swift
import Foundation

class JogoAdivinhacao {
    var pontuacao: Int
    var numeroAleatorio: Int
    var nivelDificuldade: Int
    
    init() {
        pontuacao = 0
        numeroAleatorio = 0
        nivelDificuldade = 1
    }
    
    func iniciarJogo() {
        print("Bem-vindo ao Jogo de Adivinhação!")
        print("O objetivo é adivinhar o número aleatório gerado.")
        print("Você ganha pontos a cada acerto e pode usar dicas para ajudar.")
        print("Vamos começar!")
        
        gerarNumeroAleatorio()
        jogar()
    }
    
    func gerarNumeroAleatorio() {
        numeroAleatorio = Int.random(in: 1...100)
    }
    
    func jogar() {
        print("\nNível de dificuldade: \(nivelDificuldade)")
        print("Pontuação atual: \(pontuacao)")
        
        print("\nDigite um número entre 1 e 100:")
        let palpite = Int(readLine()!) ?? 0
        
        if palpite == numeroAleatorio {
            print("Parabéns! Você acertou o número!")
            pontuacao += 10
            proximoNivel()
        } else {
            print("Você errou. O número correto é \(numeroAleatorio).")
            dica()
            jogar()
        }
    }
    
    func dica() {
        if nivelDificuldade == 1 {
            if numeroAleatorio % 2 == 0 {
                print("Dica: O número é par.")
            } else {
                print("Dica: O número é ímpar.")
            }
        } else if nivelDificuldade == 2 {
            if numeroAleatorio < 50 {
                print("Dica: O número é menor que 50.")
            } else {
                print("Dica: O número é maior que 50.")
            }
        } else {
            print("Dica: O número é divisível por \(nivelDificuldade).")
        }
    }
    
    func proximoNivel() {
        nivelDificuldade += 1
        gerarNumeroAleatorio()
        jogar()
    }
}

let jogo = JogoAdivinhacao()
jogo.iniciarJogo()
```

Explicação:

- A classe `JogoAdivinhacao` é responsável por gerenciar o jogo. Ela possui propriedades como `pontuacao` (para armazenar a pontuação do jogador), `numeroAleatorio` (o número que o jogador deve adivinhar) e `nivelDificuldade` (indica o nível atual do jogo).
- O método `iniciarJogo()` exibe uma breve introdução ao jogo e chama os métodos `gerarNumeroAleatorio()` e `jogar()`.
- O método `gerarNumeroAleatorio()` gera um número aleatório entre 1 e 100 e atribui à propriedade `numeroAleatorio`.
- O método `jogar()` é responsável por solicitar o palpite do jogador, comparar com o número aleatório e tomar as ações necessárias com base no resultado.
- O método `dica()` fornece uma dica ao jogador com base no nível de dificuldade atual e no número aleatório.
- O método `proximoNivel()` aumenta o nível de dificuldade, gera um novo número aleatório e chama o método `jogar()` para continuar o jogo.
- Por fim, é criada uma instância da classe `JogoAdivinhacao` chamada `jogo`, e o método `iniciarJogo()` é chamado para iniciar o jogo.

Esse código cria um jogo de adivinhação com funcionalidades adicionais, como pontuação, níveis de dificuldade e dicas. O jogador ganha pontos a cada acerto e pode progredir para níveis mais difíceis.