```scala
// Importamos las librerías necesarias
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn
import scala.util.Random

// Creamos una clase llamada Juego que contendrá el código del juego
class Juego {

  // Definimos los atributos de la clase
  var tablero: Array[Array[Int]] = Array.ofDim[Int](3, 3) // Tablero del juego
  var jugadorActual: Int = 1 // Jugador actual (1 = jugador 1, 2 = jugador 2)
  var ganador: Int = 0 // Ganador del juego (0 = empate, 1 = jugador 1, 2 = jugador 2)

  // Método para inicializar el juego
  def inicializarJuego(): Unit = {
    // Inicializamos el tablero con valores vacíos
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        tablero(i)(j) = 0
      }
    }

    // Asignamos el jugador actual al jugador 1
    jugadorActual = 1

    // Asignamos el ganador al valor por defecto (0)
    ganador = 0
  }

  // Método para mostrar el tablero por consola
  def mostrarTablero(): Unit = {
    println("-------------")
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        if (tablero(i)(j) == 0) {
          print(" |   ")
        } else if (tablero(i)(j) == 1) {
          print(" | X ")
        } else if (tablero(i)(j) == 2) {
          print(" | O ")
        }
      }
      println(" |")
      println("-------------")
    }
  }

  // Método para obtener la jugada del jugador actual
  def obtenerJugada(): (Int, Int) = {
    var fila: Int = 0
    var columna: Int = 0

    // Pedimos al jugador actual que introduzca la fila y la columna de su jugada
    println(s"Jugador ${jugadorActual}, introduce la fila y la columna de tu jugada (separadas por un espacio):")
    val jugada = StdIn.readLine().split(" ")

    // Convertimos la fila y la columna a enteros
    fila = jugada(0).toInt
    columna = jugada(1).toInt

    // Devolvemos la fila y la columna de la jugada
    return (fila, columna)
  }

  // Método para comprobar si una jugada es válida
  def esJugadaValida(fila: Int, columna: Int): Boolean = {
    // Comprobamos si la fila y la columna están dentro del rango válido
    if (fila < 0 || fila > 2 || columna < 0 || columna > 2) {
      return false
    }

    // Comprobamos si la casilla está vacía
    if (tablero(fila)(columna) != 0) {
      return false
    }

    // Si la fila, la columna y la casilla son válidas, devolvemos true
    return true
  }

  // Método para realizar una jugada
  def realizarJugada(fila: Int, columna: Int): Unit = {
    // Asignamos el valor del jugador actual a la casilla correspondiente del tablero
    tablero(fila)(columna) = jugadorActual

    // Cambiamos el jugador actual al otro jugador
    if (jugadorActual == 1) {
      jugadorActual = 2
    } else {
      jugadorActual = 1
    }
  }

  // Método para comprobar si hay un ganador
  def hayGanador(): Int = {
    // Comprobamos si hay un ganador en las filas
    for (i <- 0 until 3) {
      if (tablero(i)(0) != 0 && tablero(i)(0) == tablero(i)(1) && tablero(i)(1) == tablero(i)(2)) {
        return tablero(i)(0)
      }
    }

    // Comprobamos si hay un ganador en las columnas