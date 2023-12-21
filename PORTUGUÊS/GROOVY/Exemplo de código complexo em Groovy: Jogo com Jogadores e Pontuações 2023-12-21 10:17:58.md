Claro! Aqui está um exemplo de código complexo em Groovy:

```groovy
import java.util.Random

// Classe para representar um Jogador
class Jogador {
    String nome
    int nivel

    // Método construtor
    Jogador(String nome, int nivel) {
        this.nome = nome
        this.nivel = nivel
    }

    // Método para calcular a pontuação do jogador
    int calcularPontuacao() {
        return nivel * new Random().nextInt(100)
    }
}

// Classe principal
class Jogo {
    List<Jogador> jogadores = []

    // Método para adicionar um jogador ao jogo
    void adicionarJogador(String nome, int nivel) {
        jogadores.add(new Jogador(nome, nivel))
    }

    // Método para exibir a pontuação de cada jogador
    void exibirPontuacao() {
        println("Pontuação dos Jogadores:")
        jogadores.each { jogador ->
            println("${jogador.nome}: ${jogador.calcularPontuacao()} pontos")
        }
    }

    // Método para encontrar o jogador com maior pontuação
    Jogador encontrarJogadorMaiorPontuacao() {
        Jogador jogadorMaiorPontuacao = jogadores[0]
        for (int i = 1; i < jogadores.size(); i++) {
            Jogador jogadorAtual = jogadores[i]
            if (jogadorAtual.calcularPontuacao() > jogadorMaiorPontuacao.calcularPontuacao()) {
                jogadorMaiorPontuacao = jogadorAtual
            }
        }
        return jogadorMaiorPontuacao
    }
}

// Exemplo de uso
def jogo = new Jogo()
jogo.adicionarJogador("João", 5)
jogo.adicionarJogador("Maria", 8)
jogo.adicionarJogador("Pedro", 3)
jogo.exibirPontuacao()
def jogadorMaiorPontuacao = jogo.encontrarJogadorMaiorPontuacao()
println("O jogador com maior pontuação é: ${jogadorMaiorPontuacao.nome}")
```

Nesse exemplo, temos um jogo onde podemos adicionar jogadores com seus respectivos níveis. Cada jogador possui um nome e um nível. A pontuação de cada jogador é calculada multiplicando o nível por um número aleatório entre 0 e 99.

O código utiliza classes para representar o conceito de jogadores e o jogo em si. A classe Jogador possui um método para calcular a pontuação e a classe Jogo possui métodos para adicionar jogadores, exibir as pontuações de todos os jogadores e encontrar o jogador com a maior pontuação.

No exemplo de uso, são adicionados três jogadores, exibida a pontuação de cada um e, por fim, é mostrado o jogador com a maior pontuação.