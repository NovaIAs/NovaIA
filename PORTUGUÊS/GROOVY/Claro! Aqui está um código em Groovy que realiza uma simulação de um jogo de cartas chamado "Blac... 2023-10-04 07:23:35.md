Claro! Aqui está um código em Groovy que realiza uma simulação de um jogo de cartas chamado "Blackjack". O código é bastante extenso e contém várias funcionalidades, como a criação do baralho, a distribuição de cartas para o jogador e a computação do valor total das cartas em sua mão. Vamos lá:

```groovy
import java.util.Random

// Definindo as constantes do jogo
def NAIPES = ['♠', '♦', '♥', '♣']
def RANKS = ['A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K']
def PONTOS = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10]
def BLACKJACK = 21
def DEALER_LIMITE = 17

// Classe para representar uma carta
class Carta {
    String naipe
    String rank
    int pontos
    
    Carta(String naipe, String rank, int pontos) {
        this.naipe = naipe
        this.rank = rank
        this.pontos = pontos
    }
}

// Classe para representar o baralho
class Baralho {
    List<Carta> cartas
    
    Baralho() {
        cartas = []
        for (String naipe : NAIPES) {
            for (int i = 0; i < RANKS.size(); i++) {
                cartas.add(new Carta(naipe, RANKS[i], PONTOS[i]))
            }
        }
    }
    
    void embaralhar() {
        Collections.shuffle(cartas)
    }
    
    Carta pegarCarta() {
        cartas.remove(0)
    }
}

// Classe para representar a mão do jogador
class Mao {
    List<Carta> cartas
    
    Mao() {
        cartas = []
    }
    
    void receberCarta(Carta carta) {
        cartas.add(carta)
    }
    
    int calcularPontos() {
        int total = 0
        int ases = 0
        
        for (Carta carta : cartas) {
            total += carta.pontos
            if (carta.rank == 'A') {
                ases++
            }
        }
        
        while (total > BLACKJACK && ases > 0) {
            total -= 10
            ases--
        }
        
        total
    }
}

// Função para iniciar o jogo
void iniciarJogo() {
    def baralho = new Baralho()
    baralho.embaralhar()
    
    def maoJogador = new Mao()
    def maoDealer = new Mao()
    
    maoJogador.receberCarta(baralho.pegarCarta())
    maoDealer.receberCarta(baralho.pegarCarta())
    maoJogador.receberCarta(baralho.pegarCarta())
    maoDealer.receberCarta(baralho.pegarCarta())
    
    def pontosJogador = maoJogador.calcularPontos()
    def pontosDealer = maoDealer.calcularPontos()
    
    println("Cartas do jogador:")
    for (Carta carta : maoJogador.cartas) {
        println("${carta.rank}${carta.naipe}")
    }
    println("Total de pontos do jogador: $pontosJogador")
    
    println("Cartas do dealer:")
    for (Carta carta : maoDealer.cartas) {
        println("${carta.rank}${carta.naipe}")
    }
    println("Total de pontos do dealer: $pontosDealer")
    
    if (pontosJogador == BLACKJACK) {
        println("Blackjack! O jogador venceu!")
    } else if (pontosDealer == BLACKJACK) {
        println("Blackjack! O dealer venceu!")
    } else {
        while (pontosJogador < BLACKJACK) {
            def opcao = lerOpcao()
            
            if (opcao == 's') {
                maoJogador.receberCarta(baralho.pegarCarta())
                pontosJogador = maoJogador.calcularPontos()
                
                println("Cartas do jogador:")
                for (Carta carta : maoJogador.cartas) {
                    println("${carta.rank}${carta.naipe}")
                }
                println("Total de pontos do jogador: $pontosJogador")
            } else {
                break
            }
        }
        
        while (pontosDealer < DEALER_LIMITE) {
            maoDealer.receberCarta(baralho.pegarCarta())
            pontosDealer = maoDealer.calcularPontos()
        }
        
        println("Cartas do dealer:")
        for (Carta carta : maoDealer.cartas) {
            println("${carta.rank}${carta.naipe}")
        }
        println("Total de pontos do dealer: $pontosDealer")
        
        if (pontosJogador > BLACKJACK) {
            println("O jogador estourou! O dealer venceu!")
        } else if (pontosDealer > BLACKJACK) {
            println("O dealer estourou! O jogador venceu!")
        } else if (pontosJogador > pontosDealer) {
            println("O jogador venceu!")
        } else if (pontosDealer > pontosJogador) {
            println("O dealer venceu!")
        } else {
            println("Empate!")
        }
    }
}

// Função para ler a opção do jogador
String lerOpcao() {
    print("Deseja pegar mais uma carta? (s/n): ")
    System.console().readLine().trim()
}

// Inicia o jogo
iniciarJogo()
```

Este código cria um jogo de Blackjack simples em Groovy. O jogo começa com a criação de um baralho, que é embaralhado. Em seguida, as cartas são distribuídas para o jogador e o dealer. O jogador pode escolher pegar mais cartas até atingir 21 pontos ou decidir parar. O dealer então pega cartas até atingir um total de 17 pontos ou mais. Depois disso, os pontos são comparados e o resultado do jogo é exibido.

Espero que este código atenda às suas expectativas! Caso precise de mais algum código, estou à disposição.
