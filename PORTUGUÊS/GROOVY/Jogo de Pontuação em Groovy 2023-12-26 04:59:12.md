Claro! Aqui está um código complexo em Groovy:

```groovy
import java.util.Random

// Classe que representa um jogador
class Jogador {
    String nome
    int idade
    int pontuacao

    Jogador(String nome, int idade) {
        this.nome = nome
        this.idade = idade
        this.pontuacao = 0
    }

    void ganharPontos(int pontos) {
        this.pontuacao += pontos
    }

    void exibirPontuacao() {
        println("${this.nome} tem ${this.pontuacao} pontos.")
    }
}

// Função que gera um número aleatório entre 1 e 100
def gerarNumeroAleatorio() {
    Random gerador = new Random()
    return gerador.nextInt(100) + 1
}

// Função principal
void jogo() {
    println("Bem-vindo ao jogo!")

    def jogador1 = new Jogador("João", 25)
    def jogador2 = new Jogador("Maria", 30)

    while (true) {
        def numeroSorteado = gerarNumeroAleatorio()

        if (numeroSorteado % 2 == 0) {
            jogador1.ganharPontos(numeroSorteado)
        } else {
            jogador2.ganharPontos(numeroSorteado)
        }

        jogador1.exibirPontuacao()
        jogador2.exibirPontuacao()

        if (jogador1.pontuacao >= 100 || jogador2.pontuacao >= 100) {
            break
        }
    }

    if (jogador1.pontuacao > jogador2.pontuacao) {
        println("${jogador1.nome} venceu o jogo!")
    } else if (jogador1.pontuacao < jogador2.pontuacao) {
        println("${jogador2.nome} venceu o jogo!")
    } else {
        println("Empate!")
    }
}

jogo()
```

Explicação do código:

1. Começamos importando a classe `Random` para gerar números aleatórios.
2. Em seguida, definimos a classe `Jogador` que possui os atributos `nome`, `idade` e `pontuacao`. Também temos um construtor para inicializar esses atributos.
3. A classe `Jogador` tem dois métodos: `ganharPontos` para adicionar pontos à pontuação do jogador e `exibirPontuacao` para exibir a pontuação atual do jogador.
4. A função `gerarNumeroAleatorio` utiliza a classe `Random` para gerar um número aleatório entre 1 e 100.
5. A função `jogo` é a função principal do nosso programa. Ela inicializa dois jogadores, `jogador1` e `jogador2`, e entra em um loop infinito.
6. Dentro do loop, geramos um número aleatório e se ele for par, o `jogador1` ganha pontos, caso contrário, o `jogador2` ganha pontos.
7. Após cada jogada, exibimos a pontuação dos jogadores.
8. O loop continua até que um dos jogadores alcance ou ultrapasse 100 pontos.
9. Após o término do jogo, comparamos as pontuações dos jogadores e exibimos o vencedor ou um empate, caso as pontuações sejam iguais.
10. Por fim, chamamos a função `jogo` para iniciar o jogo.

Esse código simula um jogo simples em que dois jogadores competem para acumular a maior pontuação. Cada jogador ganha pontos de acordo com um número aleatório gerado. O jogo continua até que um dos jogadores alcance ou ultrapasse 100 pontos. No final, exibimos o vencedor ou um empate.