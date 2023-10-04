Claro! Aqui está um código complexo em Scala que simula um jogo de cartas chamado "Blackjack". O código está dividido em diferentes classes e métodos para melhor organização e reutilização de código.

```scala
import scala.collection.mutable.ListBuffer
import scala.util.Random

object Blackjack {

  def main(args: Array[String]): Unit = {
    val jogo = new Jogo()
    jogo.iniciar()
  }
}

class Jogo {
  private val baralho = Baralho()
  private val jogador = Jogador()
  private val dealer = Dealer()

  def iniciar(): Unit = {
    println("Bem-vindo ao Blackjack!")
    baralho.embaralhar()
    jogador.receberCarta(baralho.pegarCarta())
    dealer.receberCarta(baralho.pegarCarta())
    jogador.receberCarta(baralho.pegarCarta())
    dealer.receberCarta(baralho.pegarCarta())

    println("Suas cartas: " + jogador.mostrarCartas())
    println("Pontuação: " + jogador.pontuacao())

    println("Carta do dealer: " + dealer.mostrarCarta())

    if (jogador.pontuacao() == 21) {
      println("Você venceu!")
      return
    }

    var continuar = true

    while (continuar) {
      print("Deseja comprar mais uma carta? (s/n): ")
      val resposta = scala.io.StdIn.readLine()

      if (resposta.equalsIgnoreCase("s")) {
        jogador.receberCarta(baralho.pegarCarta())
        println("Suas cartas: " + jogador.mostrarCartas())
        println("Pontuação: " + jogador.pontuacao())

        if (jogador.pontuacao() > 21) {
          println("Você estourou! Pontuação maior que 21. Você perdeu.")
          continuar = false
        }
      } else {
        continuar = false
      }
    }

    if (jogador.pontuacao() <= 21) {
      println("Cartas do dealer: " + dealer.mostrarCartas())
      println("Pontuação do dealer: " + dealer.pontuacao())

      while (dealer.pontuacao() < 17) {
        dealer.receberCarta(baralho.pegarCarta())
        println("Dealer comprou uma carta.")
        println("Cartas do dealer: " + dealer.mostrarCartas())
        println("Pontuação do dealer: " + dealer.pontuacao())
      }

      if (dealer.pontuacao() > 21) {
        println("Dealer estourou! Pontuação maior que 21. Você venceu!")
      } else if (jogador.pontuacao() > dealer.pontuacao()) {
        println("Você venceu!")
      } else if (jogador.pontuacao() < dealer.pontuacao()) {
        println("Você perdeu.")
      } else {
        println("Empate!")
      }
    }
  }
}

case class Carta(naipe: String, valor: String)

class Baralho {
  private val naipes = List("Copas", "Espadas", "Ouros", "Paus")
  private val valores = List("Ás", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Valete", "Dama", "Rei")
  private var baralho = ListBuffer[Carta]()

  def embaralhar(): Unit = {
    baralho.clear()
    for (naipe <- naipes; valor <- valores) {
      baralho += Carta(naipe, valor)
    }
    baralho = Random.shuffle(baralho)
  }

  def pegarCarta(): Carta = {
    val carta = baralho.head
    baralho -= carta
    carta
  }
}

class Jogador {
  private val cartas = ListBuffer[Carta]()

  def receberCarta(carta: Carta): Unit = {
    cartas += carta
  }

  def mostrarCartas(): String = {
    cartas.map(carta => carta.valor + " de " + carta.naipe).mkString(", ")
  }

  def pontuacao(): Int = {
    var pontos = 0
    var ases = 0

    for (carta <- cartas) {
      carta.valor match {
        case "Ás" => {
          pontos += 11
          ases += 1
        }
        case "2" => pontos += 2
        case "3" => pontos += 3
        case "4" => pontos += 4
        case "5" => pontos += 5
        case "6" => pontos += 6
        case "7" => pontos += 7
        case "8" => pontos += 8
        case "9" => pontos += 9
        case "10" | "Valete" | "Dama" | "Rei" => pontos += 10
      }
    }

    while (pontos > 21 && ases > 0) {
      pontos -= 10
      ases -= 1
    }

    pontos
  }
}

class Dealer {
  private val cartas = ListBuffer[Carta]()

  def receberCarta(carta: Carta): Unit = {
    cartas += carta
  }

  def mostrarCarta(): String = {
    cartas.head.valor + " de " + cartas.head.naipe
  }

  def mostrarCartas(): String = {
    cartas.map(carta => carta.valor + " de " + carta.naipe).mkString(", ")
  }

  def pontuacao(): Int = {
    var pontos = 0
    var ases = 0

    for (carta <- cartas) {
      carta.valor match {
        case "Ás" => {
          pontos += 11
          ases += 1
        }
        case "2" => pontos += 2
        case "3" => pontos += 3
        case "4" => pontos += 4
        case "5" => pontos += 5
        case "6" => pontos += 6
        case "7" => pontos += 7
        case "8" => pontos += 8
        case "9" => pontos += 9
        case "10" | "Valete" | "Dama" | "Rei" => pontos += 10
      }
    }

    while (pontos > 21 && ases > 0) {
      pontos -= 10
      ases -= 1
    }

    pontos
  }
}
```

Este código implementa um jogo básico de Blackjack em Scala. O jogo começa embaralhando o baralho e distribuindo duas cartas para o jogador e duas para o dealer. O jogador pode então optar por comprar mais cartas ou parar. Em seguida, o dealer compra cartas até atingir uma pontuação de 17 ou mais. Depois disso, as pontuações são comparadas e o resultado é exibido.

O código é dividido em diferentes classes para a melhor organização do jogo. A classe `Jogo` é a classe principal que inicia o jogo e controla o fluxo do jogo. A classe `Baralho` é responsável por criar o baralho, embaralhar as cartas e fornecer cartas para os jogadores. A classe `Jogador` representa o jogador do jogo e armazena as cartas recebidas. A classe `Dealer` representa o dealer do jogo e também armazena as cartas recebidas.

O código utiliza estruturas de repetição, condicionais e manipulação de listas para implementar as regras do jogo. Ele também faz uso de entrada e saída de dados para interagir com o jogador durante o jogo.

Espero que isso atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais ajuda, é só me avisar.