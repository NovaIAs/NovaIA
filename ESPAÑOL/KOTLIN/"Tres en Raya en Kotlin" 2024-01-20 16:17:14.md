```kotlin
// Importamos las bibliotecas necesarias
import java.util.*
import kotlin.collections.ArrayList

// Creamos una clase que representa un tablero de juego
class Tablero {

    // Definimos las propiedades del tablero
    private val filas = 3
    private val columnas = 3
    private var tablero = Array(filas) { Array(columnas) { 0 } }

    // Obtenemos el valor de una celda
    fun getValor(fila: Int, columna: Int): Int {
        return tablero[fila][columna]
    }

    // Establecemos el valor de una celda
    fun setValor(fila: Int, columna: Int, valor: Int) {
        tablero[fila][columna] = valor
    }

    // Comprobamos si el juego ha terminado
    fun haTerminado(): Boolean {
        // Comprobamos si hay tres en raya en alguna fila
        for (i in 0 until filas) {
            if (tablero[i][0] != 0 && tablero[i][0] == tablero[i][1] && tablero[i][1] == tablero[i][2]) {
                return true
            }
        }

        // Comprobamos si hay tres en raya en alguna columna
        for (i in 0 until columnas) {
            if (tablero[0][i] != 0 && tablero[0][i] == tablero[1][i] && tablero[1][i] == tablero[2][i]) {
                return true
            }
        }

        // Comprobamos si hay tres en raya en alguna diagonal
        if (tablero[0][0] != 0 && tablero[0][0] == tablero[1][1] && tablero[1][1] == tablero[2][2]) {
            return true
        }
        if (tablero[0][2] != 0 && tablero[0][2] == tablero[1][1] && tablero[1][1] == tablero[2][0]) {
            return true
        }

        // Comprobamos si el tablero está lleno
        var lleno = true
        for (i in 0 until filas) {
            for (j in 0 until columnas) {
                if (tablero[i][j] == 0) {
                    lleno = false
                    break
                }
            }
            if (!lleno) {
                break
            }
        }

        return lleno
    }

    // Obtenemos el ganador del juego
    fun getGanador(): Int {
        // Comprobamos si hay tres en raya en alguna fila
        for (i in 0 until filas) {
            if (tablero[i][0] != 0 && tablero[i][0] == tablero[i][1] && tablero[i][1] == tablero[i][2]) {
                return tablero[i][0]
            }
        }

        // Comprobamos si hay tres en raya en alguna columna
        for (i in 0 until columnas) {
            if (tablero[0][i] != 0 && tablero[0][i] == tablero[1][i] && tablero[1][i] == tablero[2][i]) {
                return tablero[0][i]
            }
        }

        // Comprobamos si hay tres en raya en alguna diagonal
        if (tablero[0][0] != 0 && tablero[0][0] == tablero[1][1] && tablero[1][1] == tablero[2][2]) {
            return tablero[0][0]
        }
        if (tablero[0][2] != 0 && tablero[0][2] == tablero[1][1] && tablero[1][1] == tablero[2][0]) {
            return tablero[0][2]
        }

        return 0
    }

    // Imprimimos el tablero
    fun imprimir() {
        for (i in 0 until filas) {
            for (j in 0 until columnas) {