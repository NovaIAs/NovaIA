Claro! Aqui está um exemplo de um código complexo em Groovy que utiliza uma abordagem de programação orientada a objetos para simular um jogo de tabuleiro chamado "Batalha Naval". O código é bastante extenso, mas é altamente diferenciado e explica cada seção em detalhes.

```groovy
import java.util.Random

class Tabuleiro {
    def tabuleiro = []
    def tamanho = 10

    Tabuleiro() {
        for (int i = 0; i < tamanho; i++) {
            def linha = []
            for (int j = 0; j < tamanho; j++) {
                linha.add("~")
            }
            tabuleiro.add(linha)
        }
    }

    void imprimirTabuleiro() {
        for (int i = 0; i < tamanho; i++) {
            for (int j = 0; j < tamanho; j++) {
                print("${tabuleiro[i][j]} ")
            }
            println()
        }
    }

    void adicionarNavio(int x, int y) {
        if (x < 0 || x >= tamanho || y < 0 || y >= tamanho) {
            println("Posição inválida!")
            return
        }

        if (tabuleiro[x][y] != "~") {
            println("Posição já ocupada!")
            return
        }

        tabuleiro[x][y] = "N"
    }

    void atirar(int x, int y) {
        if (x < 0 || x >= tamanho || y < 0 || y >= tamanho) {
            println("Posição inválida!")
            return
        }

        if (tabuleiro[x][y] == "X" || tabuleiro[x][y] == "O") {
            println("Você já atirou nessa posição!")
            return
        }

        if (tabuleiro[x][y] == "N") {
            println("Você acertou um navio!")
            tabuleiro[x][y] = "X"
        } else {
            println("Você errou!")
            tabuleiro[x][y] = "O"
        }
    }
}

def random = new Random()
def tabuleiro = new Tabuleiro()

println("Bem-vindo à Batalha Naval!")

while (true) {
    println()
    println("O que você deseja fazer?")
    println("1. Imprimir tabuleiro")
    println("2. Adicionar um navio")
    println("3. Atirar")
    println("4. Sair")

    def opcao = Integer.parseInt(System.console().readLine())

    switch (opcao) {
        case 1:
            tabuleiro.imprimirTabuleiro()
            break
        case 2:
            println("Digite a posição x do navio:")
            def x = Integer.parseInt(System.console().readLine())
            println("Digite a posição y do navio:")
            def y = Integer.parseInt(System.console().readLine())
            tabuleiro.adicionarNavio(x, y)
            break
        case 3:
            println("Digite a posição x do tiro:")
            def tiroX = Integer.parseInt(System.console().readLine())
            println("Digite a posição y do tiro:")
            def tiroY = Integer.parseInt(System.console().readLine())
            tabuleiro.atirar(tiroX, tiroY)
            break
        case 4:
            println("Até logo!")
            return
        default:
            println("Opção inválida!")
            break
    }
}
```

Explicação do código:

1. A classe `Tabuleiro` é responsável por representar e manipular o tabuleiro do jogo. Ela possui uma matriz `tabuleiro` que armazena os símbolos dos navios e tiros, e o tamanho do tabuleiro.

2. O construtor da classe `Tabuleiro` inicializa o tabuleiro com um tamanho fixo e preenche todas as posições com o símbolo "~".

3. O método `imprimirTabuleiro` percorre o tabuleiro e imprime os símbolos de cada posição.

4. O método `adicionarNavio` recebe as coordenadas (x, y) e verifica se a posição é válida e se está livre. Se for válida e livre, o símbolo "N" é atribuído à posição.

5. O método `atirar` recebe as coordenadas (x, y) do tiro e verifica se a posição é válida e se já foi atirado. Se for válida e não tiver sido atirado, o símbolo é atualizado para "X" se acertar um navio ou "O" se errar.

6. No bloco principal, é criado um objeto `Tabuleiro` e é exibido um menu com opções para imprimir o tabuleiro, adicionar um navio, atirar ou sair do jogo.

7. Dependendo da opção escolhida, os métodos apropriados da classe `Tabuleiro` são chamados e as ações são realizadas.

8. O loop principal continua até que a opção "Sair" seja escolhida.

Espero que este código atenda às suas expectativas!