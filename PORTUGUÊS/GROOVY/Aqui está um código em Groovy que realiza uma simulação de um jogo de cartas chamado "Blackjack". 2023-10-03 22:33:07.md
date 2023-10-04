Claro! Aqui está um código em Groovy que realiza uma simulação de um jogo de cartas chamado "Blackjack". O objetivo do jogo é obter uma pontuação maior que a do "dealer" (a casa) sem ultrapassar 21 pontos. Este código é bastante complexo e possui várias funcionalidades, como a criação do baralho, a distribuição de cartas para o jogador e para a casa, o cálculo da pontuação e a lógica para determinar o vencedor. Vamos lá:

```groovy
class Carta {
    String valor
    String naipe

    Carta(String valor, String naipe) {
        this.valor = valor
        this.naipe = naipe
    }
}

class Baralho {
    List<Carta> cartas

    Baralho() {
        cartas = []
        def naipes = ['Paus', 'Copas', 'Espadas', 'Ouros']
        def valores = ['Ás', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'Valete', 'Dama', 'Rei']

        valores.each { valor ->
            naipes.each { naipe ->
                cartas.add(new Carta(valor, naipe))
            }
        }
    }

    Carta pegarCarta() {
        def carta = cartas.get(new Random().nextInt(cartas.size()))
        cartas.remove(carta)
        return carta
    }
}

class Jogador {
    String nome
    List<Carta> mao

    Jogador(String nome) {
        this.nome = nome
        this.mao = []
    }

    void receberCarta(Carta carta) {
        mao.add(carta)
    }

    int calcularPontuacao() {
        int pontuacao = 0
        int ases = 0

        mao.each { carta ->
            switch (carta.valor) {
                case 'Ás':
                    pontuacao += 11
                    ases++
                    break
                case 'Valete':
                case 'Dama':
                case 'Rei':
                    pontuacao += 10
                    break
                default:
                    pontuacao += Integer.parseInt(carta.valor)
                    break
            }
        }

        while (pontuacao > 21 && ases > 0) {
            pontuacao -= 10
            ases--
        }

        pontuacao
    }
}

class Jogo {
    Baralho baralho
    Jogador jogador
    Jogador dealer

    Jogo(String nomeJogador) {
        baralho = new Baralho()
        jogador = new Jogador(nomeJogador)
        dealer = new Jogador("Dealer")
    }

    void iniciar() {
        println("Bem-vindo ao Blackjack, ${jogador.nome}!")
        println("Vamos começar!\n")

        jogador.receberCarta(baralho.pegarCarta())
        dealer.receberCarta(baralho.pegarCarta())
        jogador.receberCarta(baralho.pegarCarta())
        dealer.receberCarta(baralho.pegarCarta())

        println("Suas cartas:")
        jogador.mao.each { carta ->
            println("${carta.valor} de ${carta.naipe}")
        }
        println("Pontuação: ${jogador.calcularPontuacao()}\n")

        println("Carta do dealer:")
        println("${dealer.mao.get(0).valor} de ${dealer.mao.get(0).naipe}\n")

        if (jogador.calcularPontuacao() == 21) {
            println("Parabéns! Você ganhou com um Blackjack!")
            return
        }

        while (jogador.calcularPontuacao() < 21) {
            print("Deseja mais uma carta? (s/n): ")
            def resposta = System.console().readLine()

            if (resposta.equalsIgnoreCase('s')) {
                jogador.receberCarta(baralho.pegarCarta())
                println("Nova carta:")
                println("${jogador.mao.last().valor} de ${jogador.mao.last().naipe}")
                println("Pontuação: ${jogador.calcularPontuacao()}\n")
            } else {
                break
            }
        }

        println("Cartas do dealer:")
        dealer.mao.each { carta ->
            println("${carta.valor} de ${carta.naipe}")
        }
        println("Pontuação: ${dealer.calcularPontuacao()}\n")

        if (dealer.calcularPontuacao() == 21) {
            println("Dealer ganhou com um Blackjack!")
        } else if (dealer.calcularPontuacao() > 21) {
            println("Dealer estourou! Você ganhou!")
        } else if (jogador.calcularPontuacao() > 21) {
            println("Você estourou! Dealer ganhou!")
        } else if (jogador.calcularPontuacao() > dealer.calcularPontuacao()) {
            println("Parabéns! Você ganhou!")
        } else if (jogador.calcularPontuacao() < dealer.calcularPontuacao()) {
            println("Dealer ganhou!")
        } else {
            println("Empate!")
        }
    }
}

def jogo = new Jogo("Jogador")
jogo.iniciar()
```

Neste código, criamos três classes: `Carta`, `Baralho` e `Jogador`. A classe `Carta` representa uma carta do baralho, com um valor e um naipe. A classe `Baralho` é responsável pela criação do baralho, com todas as suas cartas, e também pela distribuição aleatória das cartas aos jogadores. A classe `Jogador` representa um jogador, que possui um nome e uma mão de cartas.

A classe `Jogo` é a principal, que controla a lógica do jogo. No método `iniciar()`, realizamos as seguintes etapas:
1. Criamos um objeto `Baralho` e dois objetos `Jogador` (um para o jogador principal e outro para o dealer).
2. Distribuímos duas cartas para cada jogador.
3. Mostramos as cartas do jogador principal e a pontuação atual.
4. Mostramos a primeira carta do dealer.
5. Verificamos se o jogador principal já possui um Blackjack (pontuação igual a 21).
6. Solicitamos ao jogador principal se ele deseja receber mais uma carta. Se sim, distribuímos uma nova carta e mostramos a pontuação atualizada. Se não, passamos para a próxima etapa.
7. Mostramos as cartas do dealer e sua pontuação.
8. Verificamos todas as possibilidades de vitória (Blackjack, estouro, pontuação maior ou menor) e exibimos a mensagem correspondente.

Espero que este código atenda às suas expectativas quanto à complexidade e funcionalidade!