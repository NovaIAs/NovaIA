```scala
// Importamos las librerías necesarias
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

// Definimos una clase para representar un tablero de ajedrez
class TableroAjedrez {

  // Creamos un array bidimensional para almacenar las piezas
  private val tablero = Array.ofDim[Pieza](8, 8)

  // Inicializamos el tablero con las piezas iniciales
  def inicializar(): Unit = {
    // Colocamos las piezas blancas
    tablero(0)(0) = Torre("blanca")
    tablero(1)(0) = Caballo("blanca")
    tablero(2)(0) = Alfil("blanca")
    tablero(3)(0) = Reina("blanca")
    tablero(4)(0) = Rey("blanca")
    tablero(5)(0) = Alfil("blanca")
    tablero(6)(0) = Caballo("blanca")
    tablero(7)(0) = Torre("blanca")
    for (i <- 0 to 7) {
      tablero(i)(1) = Peon("blanca")
    }

    // Colocamos las piezas negras
    tablero(0)(7) = Torre("negra")
    tablero(1)(7) = Caballo("negra")
    tablero(2)(7) = Alfil("negra")
    tablero(3)(7) = Reina("negra")
    tablero(4)(7) = Rey("negra")
    tablero(5)(7) = Alfil("negra")
    tablero(6)(7) = Caballo("negra")
    tablero(7)(7) = Torre("negra")
    for (i <- 0 to 7) {
      tablero(i)(6) = Peon("negra")
    }
  }

  // Obtenemos la pieza en una posición determinada
  def getPieza(x: Int, y: Int): Pieza = {
    tablero(x)(y)
  }

  // Movemos una pieza de una posición a otra
  def moverPieza(x1: Int, y1: Int, x2: Int, y2: Int): Unit = {
    // Obtenemos la pieza que queremos mover
    val pieza = getPieza(x1, y1)

    // Comprobamos si la pieza es válida
    if (pieza != null) {
      // Comprobamos si el movimiento es válido
      if (pieza.esMovimientoValido(x1, y1, x2, y2)) {
        // Movemos la pieza
        tablero(x2)(y2) = pieza
        tablero(x1)(y1) = null
      }
    }
  }

  // Imprimimos el tablero
  def imprimir(): Unit = {
    for (i <- 0 to 7) {
      for (j <- 0 to 7) {
        print(tablero(i)(j) + " ")
      }
      println()
    }
  }
}

// Definimos una clase para representar una pieza de ajedrez
abstract class Pieza {

  // El color de la pieza
  val color: String

  // El símbolo de la pieza
  val simbolo: String

  // Comprueba si un movimiento es válido
  def esMovimientoValido(x1: Int, y1: Int, x2: Int, y2: Int): Boolean

}

// Definimos una clase para representar una torre
class Torre(override val color: String) extends Pieza {

  // El símbolo de la torre
  override val simbolo: String = "\u265C"

  // Comprueba si un movimiento es válido
  override def esMovimientoValido(x1: Int, y1: Int, x2: Int, y2: Int): Boolean = {
    // La torre solo puede moverse en horizontal o vertical
    (x1 == x2 && y1 != y2) || (y1 == y2 && x1 != x2)
  }
}

// Definimos una clase para representar un caballo
class Caballo(override val color: String) extends Pieza {

  // El símbolo del caballo
  override val simbolo: String = "\u265E"

  // Comprueba si un movimiento es válido
  override def esMovimientoValido(x1: Int, y1: Int, x2: Int, y2: Int): Boolean = {
    // El caballo solo puede moverse en forma de "L"
    (Math.abs(x1 - x2) == 2 && Math.abs(y1 - y2) == 1) || (Math.abs(x1 - x2) == 1 && Math.abs(y1 - y2) == 2)
  }
}

// Definimos una clase para representar un alfil
class Alfil(override val color: String) extends Pieza {

  // El símbolo del alfil
  override val simbolo: String = "\u265D"

  // Comprueba si un movimiento es válido
  override def esMovimientoValido(x1: Int, y1: Int, x2: Int, y2: Int): Boolean = {
    // El alfil solo puede moverse en diagonal
    Math.abs(x1 - x2) == Math.abs(y1 - y2)
  }
}

// Definimos una clase para representar una reina
class Reina(override val color: String) extends Pieza {

  // El símbolo de la reina
  override val simbolo: String = "\u265B"

  // Comprueba si un movimiento es válido
  override def esMovimientoValido(x1: Int, y1: Int, x2: Int, y2: Int): Boolean = {
    // La reina puede moverse en cualquier dirección
    (x1 == x2 && y1 != y2) || (y1 == y2 && x1 != x2) || (Math.abs(x1 - x2) == Math.abs(y1 - y2))
  }
}

// Definimos una clase para representar un rey
class Rey(override val color: String) extends Pieza {

  // El símbolo del rey
  override val simbolo: String = "\u265A"

  // Comprueba si un movimiento es válido
  override def esMovimientoValido(x1: Int, y1: Int, x2: Int, y2: Int): Boolean = {
    // El rey solo puede moverse una casilla en cualquier dirección
    Math.abs(x1 - x2) <= 1 && Math.abs(y1 - y2) <= 1
  }
}

// Definimos una clase para representar un peón
class Peon(override val color: String) extends Pieza {

  // El símbolo del peón
  override val simbolo: String = "\u265F"

  // Comprueba si un movimiento es válido
  override def esMovimientoValido(x1: Int, y1: Int, x2: Int, y2: Int): Boolean {
    // El peón solo puede moverse hacia adelante una casilla, o dos casillas si está en su primera fila
    (color == "blanca" && ((y2 - y1 == 1) || (y1 == 1 && y2 == 3))) ||
      (color == "negra" && ((y1 - y2 == 1) || (y1 == 6 && y2 == 4)))
  }
}

// Definimos una clase para representar un jugador
class Jugador {

  // El nombre del jugador
  val nombre: String

  // El color de las piezas del jugador
  val color: String

  // Constructor de la clase
  def Jugador(nombre: String, color: String): Unit = {
    this.nombre = nombre
    this.color = color
  }
}

// Definimos una clase para representar una partida de ajedrez
class PartidaAjedrez {

  // El tablero de ajedrez
  val tablero = new TableroAjedrez

  // Los jugadores de la partida