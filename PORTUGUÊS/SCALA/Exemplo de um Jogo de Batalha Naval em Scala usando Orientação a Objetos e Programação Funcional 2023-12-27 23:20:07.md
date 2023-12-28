Aqui está um exemplo de um código complexo em Scala, que usa programação funcional e orientada a objetos para lidar com uma simulação de um jogo de tabuleiro chamado "Batalha Naval". O código faz uso de classes, traits, padrões de design e funções avançadas para criar uma experiência de jogo interativa.

```scala
import scala.util.Random

// Definindo o tamanho do tabuleiro
val tamanhoTabuleiro = 10

// Definindo os tipos de navios
sealed trait TipoNavio {
  def tamanho: Int
}

case object PortaAvioes extends TipoNavio {
  val tamanho = 5
}

case object Encouracado extends TipoNavio {
  val tamanho = 4
}

case object Cruzador extends TipoNavio {
  val tamanho = 3
}

case object Submarino extends TipoNavio {
  val tamanho = 3
}

case object Destroier extends TipoNavio {
  val tamanho = 2
}

// Definindo a posição do navio no tabuleiro
case class Posicao(x: Int, y: Int)

// Definindo a orientação do navio no tabuleiro
sealed trait Orientacao

case object Horizontal extends Orientacao

case object Vertical extends Orientacao

// Definindo a classe Navio
case class Navio(tipo: TipoNavio, posicao: Posicao, orientacao: Orientacao) {
  def posicoes: List[Posicao] = {
    val delta = if (orientacao == Horizontal) Posicao(1, 0) else Posicao(0, 1)
    (0 until tipo.tamanho).map(i => Posicao(posicao.x + delta.x * i, posicao.y + delta.y * i)).toList
  }
}

// Definindo o tabuleiro
class Tabuleiro {
  private val tabuleiro: Array[Array[Boolean]] = Array.fill(tamanhoTabuleiro, tamanhoTabuleiro)(false)

  def posicaoValida(posicao: Posicao): Boolean = {
    posicao.x >= 0 && posicao.x < tamanhoTabuleiro && posicao.y >= 0 && posicao.y < tamanhoTabuleiro
  }

  def posicaoLivre(posicao: Posicao): Boolean = {
    posicaoValida(posicao) && !tabuleiro(posicao.x)(posicao.y)
  }

  def posicoesLivres(navio: Navio): Boolean = {
    navio.posicoes.forall(posicaoLivre)
  }

  def colocarNavio(navio: Navio): Unit = {
    navio.posicoes.foreach(posicao => tabuleiro(posicao.x)(posicao.y) = true)
  }

  def imprimir(): Unit = {
    for (i <- 0 until tamanhoTabuleiro) {
      for (j <- 0 until tamanhoTabuleiro) {
        if (tabuleiro(i)(j)) print("X ")
        else print(". ")
      }
      println()
    }
  }
}

// Definindo a classe Jogador
class Jogador(val nome: String) {
  private val tabuleiro: Tabuleiro = new Tabuleiro
  private var navios: List[Navio] = List.empty

  def adicionarNavio(navio: Navio): Boolean = {
    if (tabuleiro.posicoesLivres(navio)) {
      tabuleiro.colocarNavio(navio)
      navios = navio :: navios
      true
    } else false
  }

  def atacar(jogador: Jogador, posicao: Posicao): Boolean = {
    if (tabuleiro.posicaoValida(posicao)) {
      val atingiuNavio = jogador.navios.exists(_.posicoes.contains(posicao))
      if (atingiuNavio) {
        jogador.navios = jogador.navios.filterNot(_.posicoes.contains(posicao))
        true
      } else false
    } else false
  }

  def imprimirTabuleiro(): Unit = {
    tabuleiro.imprimir()
  }
}

// Função para gerar uma posição aleatória
def gerarPosicaoAleatoria(): Posicao = {
  val random = new Random
  Posicao(random.nextInt(tamanhoTabuleiro), random.nextInt(tamanhoTabuleiro))
}

// Função para gerar uma orientação aleatória
def gerarOrientacaoAleatoria(): Orientacao = {
  val random = new Random
  if (random.nextBoolean()) Horizontal else Vertical
}

// Criando dois jogadores
val jogador1 = new Jogador("Jogador 1")
val jogador2 = new Jogador("Jogador 2")

// Adicionando navios aleatórios para cada jogador
val naviosJogador1 = List(PortaAvioes, Encouracado, Cruzador, Submarino, Destroier)
naviosJogador1.foreach { navio =>
  var posicao = gerarPosicaoAleatoria()
  var orientacao = gerarOrientacaoAleatoria()
  while (!jogador1.adicionarNavio(Navio(navio, posicao, orientacao))) {
    posicao = gerarPosicaoAleatoria()
    orientacao = gerarOrientacaoAleatoria()
  }
}

val naviosJogador2 = List(PortaAvioes, Encouracado, Cruzador, Submarino, Destroier)
naviosJogador2.foreach { navio =>
  var posicao = gerarPosicaoAleatoria()
  var orientacao = gerarOrientacaoAleatoria()
  while (!jogador2.adicionarNavio(Navio(navio, posicao, orientacao))) {
    posicao = gerarPosicaoAleatoria()
    orientacao = gerarOrientacaoAleatoria()
  }
}

// Iniciando o jogo
var jogadorAtual = jogador1
var jogoAcabou = false

while (!jogoAcabou) {
  jogadorAtual.imprimirTabuleiro()
  println(s"Vez de ${jogadorAtual.nome}")
  val posicaoAtaque = gerarPosicaoAleatoria()
  val jogadorAlvo = if (jogadorAtual == jogador1) jogador2 else jogador1
  val atingiuNavio = jogadorAtual.atacar(jogadorAlvo, posicaoAtaque)
  if (atingiuNavio) {
    println("Acertou um navio!")
    if (jogadorAlvo.navios.isEmpty) {
      println(s"${jogadorAtual.nome} venceu o jogo!")
      jogoAcabou = true
    }
  } else {
    println("Errou o ataque!")
    jogadorAtual = jogadorAlvo
  }
}
```

Este código implementa um jogo de Batalha Naval onde dois jogadores competem para afundar os navios um do outro. Os jogadores posicionam seus navios no tabuleiro e, em seguida, alternam entre atacar e defender até que todos os navios de um jogador sejam afundados.

O código começa definindo os tipos de navios, suas posições e orientações. Em seguida, é criada a classe Tabuleiro, que contém funções para verificar a validade e disponibilidade das posições no tabuleiro, além de imprimir o tabuleiro na tela.

A classe Jogador é definida com um nome, um tabuleiro e uma lista de navios. Ela possui métodos para adicionar navios, atacar outros jogadores e imprimir seu tabuleiro.

A função `gerarPosicaoAleatoria()` e `gerarOrientacaoAleatoria()` são usadas para gerar posições e orientações aleatórias para os navios dos jogadores.

Em seguida, são criados os jogadores e seus navios são gerados aleatoriamente. O jogo começa com o jogador 1 e, em cada turno, um jogador ataca o outro até que todos os navios de um jogador sejam afundados. O jogo termina quando um jogador vence.

Este código é um exemplo de como um jogo de Batalha Naval pode ser implementado em Scala, usando programação orientada a objetos e funcional. Ele demonstra o uso de classes, traits, padrões de design e funções avançadas para criar um jogo interativo.