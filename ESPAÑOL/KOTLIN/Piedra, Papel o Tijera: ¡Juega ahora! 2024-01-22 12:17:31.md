```kotlin
// Este código Kotlin crea un juego de piedra, papel o tijera con una interfaz de usuario gráfica (GUI).

// Importamos las librerías necesarias.
import java.awt.BorderLayout
import java.awt.Dimension
import java.awt.GridLayout
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import javax.swing.*

// Creamos la clase principal del juego.
class PiedraPapelTijera : JFrame() {

    // Creamos los componentes de la GUI.
    private val panelPrincipal = JPanel()
    private val panelBotones = JPanel()
    private val botonPiedra = JButton("Piedra")
    private val botonPapel = JButton("Papel")
    private val botonTijera = JButton("Tijera")
    private val panelResultado = JPanel()
    private val etiquetaResultado = JLabel("¡Juega ahora!")

    // Inicializamos el juego.
    init {
        // Configuramos la ventana principal.
        title = "Piedra, Papel o Tijera"
        size = Dimension(300, 200)
        defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        isResizable = false

        // Creamos el panel principal.
        panelPrincipal.layout = BorderLayout()

        // Creamos el panel de botones.
        panelBotones.layout = GridLayout(1, 3)
        panelBotones.add(botonPiedra)
        panelBotones.add(botonPapel)
        panelBotones.add(botonTijera)

        // Creamos el panel de resultados.
        panelResultado.layout = BorderLayout()
        panelResultado.add(etiquetaResultado, BorderLayout.CENTER)

        // Añadimos los paneles al panel principal.
        panelPrincipal.add(panelBotones, BorderLayout.CENTER)
        panelPrincipal.add(panelResultado, BorderLayout.SOUTH)

        // Añadimos el panel principal a la ventana principal.
        contentPane = panelPrincipal

        // Añadimos los controladores de eventos a los botones.
        botonPiedra.addActionListener(ActionListener { jugar(1) })
        botonPapel.addActionListener(ActionListener { jugar(2) })
        botonTijera.addActionListener(ActionListener { jugar(3) })
    }

    // Método que se ejecuta cuando se hace clic en un botón.
    private fun jugar(eleccionJugador: Int) {
        // Generamos una elección aleatoria para la computadora.
        val eleccionComputadora = (1..3).random()

        // Determinamos el resultado del juego.
        val resultado = cuando {
            eleccionJugador == eleccionComputadora -> 0 // Empate
            eleccionJugador == 1 && eleccionComputadora == 2 -> 1 // Gana la computadora
            eleccionJugador == 1 && eleccionComputadora == 3 -> -1 // Gana el jugador
            eleccionJugador == 2 && eleccionComputadora == 1 -> -1 // Gana el jugador
            eleccionJugador == 2 && eleccionComputadora == 3 -> 1 // Gana la computadora
            eleccionJugador == 3 && eleccionComputadora == 1 -> 1 // Gana la computadora
            eleccionJugador == 3 && eleccionComputadora == 2 -> -1 // Gana el jugador
            else -> 0 // Empate
        }

        // Mostramos el resultado del juego.
        etiquetaResultado.text = when (resultado) {
            0 -> "Empate"
            1 -> "Pierdes"
            -1 -> "¡Ganaste!"
            else -> "Error"
        }
    }
}

// Creamos una instancia del juego y la ejecutamos.
fun main(args: Array<String>) {
    PiedraPapelTijera().isVisible = true
}
```

Explicación del código:

* La clase `PiedraPapelTijera` es la clase principal del juego.
* La variable `panelPrincipal` es el panel principal del juego.
* La variable `panelBotones` es el panel que contiene los botones de piedra, papel y tijera.
* Las variables `botonPiedra`, `botonPapel` y `botonTijera` son los botones de piedra, papel y tijera, respectivamente.
* La variable `panelResultado` es el panel que contiene la etiqueta de resultado.
* La variable `etiquetaResultado` es la etiqueta que muestra el resultado del juego.
* El método `init` se ejecuta cuando se instancia la clase `PiedraPapelTijera`.
* El método `jugar` se ejecuta cuando se hace clic en un botón.
* El método `main` crea una instancia del juego y la ejecuta.

El juego funciona de la siguiente manera:

1. El jugador hace clic en un botón para elegir piedra, papel o tijera.
2. El juego genera una elección aleatoria para la computadora.
3. El juego determina el resultado del juego en función de las elecciones del jugador y la computadora.
4. El juego muestra el resultado del juego en la etiqueta de resultado.