```javascript
// Definición de la clase Jugador
class Jugador {
  constructor(nombre, posicion, numeroCamiseta) {
    this.nombre = nombre;
    this.posicion = posicion;
    this.numeroCamiseta = numeroCamiseta;
  }

  // Método para imprimir la información del jugador
  mostrarInformacion() {
    console.log(
      `Nombre: ${this.nombre}, Posición: ${this.posicion}, Número de Camiseta: ${this.numeroCamiseta}`
    );
  }
}

// Creación de una lista de jugadores
const jugadores = [
  new Jugador("Cristiano Ronaldo", "Delantero", 7),
  new Jugador("Lionel Messi", "Delantero", 10),
  new Jugador("Neymar Jr.", "Delantero", 11),
  new Jugador("Sergio Ramos", "Defensa", 4),
  new Jugador("Virgil van Dijk", "Defensa", 4)
];

// Función para obtener el jugador con mayor número de goles
const getJugadorConMasGoles = (jugadores) => {
  let jugadorConMasGoles = jugadores[0];
  for (let i = 1; i < jugadores.length; i++) {
    if (jugadores[i].goles > jugadorConMasGoles.goles) {
      jugadorConMasGoles = jugadores[i];
    }
  }
  return jugadorConMasGoles;
};

// Función para obtener el jugador con menor número de goles
const getJugadorConMenosGoles = (jugadores) => {
  let jugadorConMenosGoles = jugadores[0];
  for (let i = 1; i < jugadores.length; i++) {
    if (jugadores[i].goles < jugadorConMenosGoles.goles) {
      jugadorConMenosGoles = jugadores[i];
    }
  }
  return jugadorConMenosGoles;
};

// Función para calcular el promedio de goles de los jugadores
const getPromedioGolesJugadores = (jugadores) => {
  let totalGoles = 0;
  for (let i = 0; i < jugadores.length; i++) {
    totalGoles += jugadores[i].goles;
  }
  return totalGoles / jugadores.length;
};

// Función para imprimir la información de los jugadores
const mostrarInformacionJugadores = (jugadores) => {
  for (let i = 0; i < jugadores.length; i++) {
    jugadores[i].mostrarInformacion();
  }
};

// Impresión de la información de los jugadores
console.log("Información de los jugadores:");
mostrarInformacionJugadores(jugadores);

// Obtención del jugador con mayor número de goles
const jugadorConMasGoles = getJugadorConMasGoles(jugadores);
console.log(`Jugador con mayor número de goles:`);
jugadorConMasGoles.mostrarInformacion();

// Obtención del jugador con menor número de goles
const jugadorConMenosGoles = getJugadorConMenosGoles(jugadores);
console.log(`Jugador con menor número de goles:`);
jugadorConMenosGoles.mostrarInformacion();

// Cálculo del promedio de goles de los jugadores
const promedioGolesJugadores = getPromedioGolesJugadores(jugadores);
console.log(`Promedio de goles de los jugadores: ${promedioGolesJugadores}`);
```

Explicación del código:

* **Definición de la clase Jugador:**
  * Esta clase define los atributos y métodos de los objetos Jugador.
  * Los atributos son: nombre, posicion y numeroCamiseta.
  * El método mostrarInformacion() imprime la información del jugador.
* **Creación de una lista de jugadores:**
  * Se crea una lista de jugadores usando la clase Jugador.
  * Cada jugador tiene un nombre, una posición y un número de camiseta.
* **Funciones para obtener el jugador con mayor número de goles, el jugador con menor número de goles y el promedio de goles de los jugadores:**
  * Estas funciones utilizan la lista de jugadores para calcular los valores solicitados.
* **Impresión de la información de los jugadores:**
  * Se utiliza la función mostrarInformacionJugadores() para imprimir la información de los jugadores.
* **Obtención del jugador con mayor y menor número de goles:**
  * Se utilizan las funciones getJugadorConMasGoles() y getJugadorConMenosGoles() para obtener los jugadores con mayor y menor número de goles.
* **Cálculo del promedio de goles de los jugadores:**
  * Se utiliza la función getPromedioGolesJugadores() para calcular el promedio de goles de los jugadores.