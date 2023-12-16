Claro! Aqui está um exemplo de um código complexo em Swift que envolve o uso de estruturas de dados, loops e funções. O código a seguir é uma implementação de um jogo de adivinhação de números, onde o jogador tenta adivinhar um número entre 1 e 100.

```
import Foundation

// Função para gerar um número aleatório entre 1 e 100
func gerarNumeroAleatorio() -> Int {
    return Int.random(in: 1...100)
}

// Função para ler a entrada do usuário
func lerEntrada() -> Int {
    guard let entrada = readLine(),
          let numero = Int(entrada) else {
        print("Entrada inválida. Por favor, digite um número.")
        return lerEntrada()
    }
    return numero
}

// Função principal do jogo
func jogarAdivinhacao() {
    let numeroCorreto = gerarNumeroAleatorio()
    var tentativas = 0
    
    print("Bem-vindo ao jogo de adivinhação!")
    print("Tente adivinhar um número entre 1 e 100.")
    
    while true {
        print("Digite o seu palpite:")
        let palpite = lerEntrada()
        tentativas += 1
        
        if palpite == numeroCorreto {
            print("Parabéns! Você acertou o número em \(tentativas) tentativas.")
            break
        } else if palpite < numeroCorreto {
            print("O número correto é maior. Tente novamente.")
        } else {
            print("O número correto é menor. Tente novamente.")
        }
    }
}

// Chamar a função principal para iniciar o jogo
jogarAdivinhacao()
```

Explicação do código:

1. A função `gerarNumeroAleatorio()` utiliza a função `Int.random(in: 1...100)` para gerar um número aleatório entre 1 e 100.

2. A função `lerEntrada()` lê a entrada do usuário utilizando `readLine()`. Em seguida, verifica se a entrada é um número válido. Caso contrário, exibe uma mensagem de erro e chama recursivamente a função para obter uma entrada válida.

3. A função `jogarAdivinhacao()` é a função principal do jogo. Ela chama a função `gerarNumeroAleatorio()` para obter um número correto aleatório e inicializa a variável `tentativas` como 0.

4. O jogo é executado em um loop infinito `while true`. Dentro do loop, o jogador é solicitado a digitar um palpite utilizando a função `lerEntrada()`. O número de tentativas é incrementado a cada iteração.

5. Se o palpite do jogador for igual ao número correto, o jogo termina e uma mensagem de parabéns é exibida, juntamente com o número de tentativas.

6. Caso contrário, se o palpite for menor que o número correto, uma mensagem informando que o número correto é maior é exibida. Caso contrário, uma mensagem informando que o número correto é menor é exibida.

7. O jogo continua até que o jogador adivinhe corretamente o número.

8. Por fim, a função `jogarAdivinhacao()` é chamada para iniciar o jogo.