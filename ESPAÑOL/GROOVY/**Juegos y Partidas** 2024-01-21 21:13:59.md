```groovy
// Definición de la clase `Juego`
class Juego {

  // Propiedades de la clase
  String nombre
  int puntuacionMaxima

  // Constructor de la clase
  def Juego(String nombre, int puntuacionMaxima) {
    this.nombre = nombre
    this.puntuacionMaxima = puntuacionMaxima
  }

  // Método que devuelve el nombre del juego
  String getNombre() {
    return nombre
  }

  // Método que devuelve la puntuación máxima del juego
  int getPuntuacionMaxima() {
    return puntuacionMaxima
  }

  // Método que imprime el nombre y la puntuación máxima del juego
  void imprimir() {
    println "Nombre: ${nombre}"
    println "Puntuación máxima: ${puntuacionMaxima}"
  }
}

// Definición de la clase `Jugador`
class Jugador {

  // Propiedades de la clase
  String nombre
  int puntuacion

  // Constructor de la clase
  def Jugador(String nombre, int puntuacion) {
    this.nombre = nombre
    this.puntuacion = puntuacion
  }

  // Método que devuelve el nombre del jugador
  String getNombre() {
    return nombre
  }

  // Método que devuelve la puntuación del jugador
  int getPuntuacion() {
    return puntuacion
  }

  // Método que imprime el nombre y la puntuación del jugador
  void imprimir() {
    println "Nombre: ${nombre}"
    println "Puntuación: ${puntuacion}"
  }
}

// Definición de la clase `Partida`
class Partida {

  // Propiedades de la clase
  Juego juego
  Jugador jugador
  int puntuacion

  // Constructor de la clase
  def Partida(Juego juego, Jugador jugador, int puntuacion) {
    this.juego = juego
    this.jugador = jugador
    this.puntuacion = puntuacion
  }

  // Método que devuelve el juego de la partida
  Juego getJuego() {
    return juego
  }

  // Método que devuelve el jugador de la partida
  Jugador getJugador() {
    return jugador
  }

  // Método que devuelve la puntuación de la partida
  int getPuntuacion() {
    return puntuacion
  }

  // Método que imprime el juego, el jugador y la puntuación de la partida
  void imprimir() {
    println "Juego: ${juego.nombre}"
    println "Jugador: ${jugador.nombre}"
    println "Puntuación: ${puntuacion}"
  }
}

// Definición de la clase `GestorPartidas`
class GestorPartidas {

  // Propiedades de la clase
  List<Partida> partidas

  // Constructor de la clase
  def GestorPartidas() {
    partidas = []
  }

  // Método que añade una partida a la lista de partidas
  void añadirPartida(Partida partida) {
    partidas.add(partida)
  }

  // Método que imprime la lista de partidas
  void imprimirPartidas() {
    for (partida in partidas) {
      partida.imprimir()
    }
  }
}

// Código principal del programa
def gestorPartidas = new GestorPartidas()

// Creamos un juego
def juego = new Juego("Super Mario Bros.", 10000)

// Creamos un jugador
def jugador = new Jugador("Mario", 9000)

// Creamos una partida
def partida = new Partida(juego, jugador, 9000)

// Añadimos la partida a la lista de partidas
gestorPartidas.añadirPartida(partida)

// Imprimimos la lista de partidas
gestorPartidas.imprimirPartidas()
```

Explicación del código:

* La clase `Juego` define las propiedades `nombre` y `puntuacionMaxima` de un juego.
* La clase `Jugador` define las propiedades `nombre` y `puntuacion` de un jugador.
* La clase `Partida` define las propiedades `juego`, `jugador` y `puntuacion` de una partida.
* La clase `GestorPartidas` define la propiedad `partidas` que es una lista de partidas.
* El código principal del programa crea un juego, un jugador, una partida y la añade a la lista de partidas.
* Finalmente, se imprime la lista de partidas.