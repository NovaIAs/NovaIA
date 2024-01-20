```javascript
// Declaramos una función auxiliar para generar un número aleatorio entre un mínimo y un máximo
function generarNumeroAleatorio(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Creamos una clase que representa una baraja de cartas
class Baraja {
  constructor() {
    this.cartas = [];
    // Generamos las cartas del 1 al 13 en cuatro palos diferentes
    for (let i = 1; i <= 13; i++) {
      for (let j = 0; j < 4; j++) {
        this.cartas.push({
          numero: i,
          palo: ["Corazones", "Diamantes", "Tréboles", "Picas"][j],
        });
      }
    }
  }

  // Barajamos las cartas de la baraja
  barajar() {
    for (let i = 0; i < this.cartas.length; i++) {
      let j = generarNumeroAleatorio(0, this.cartas.length - 1);
      [this.cartas[i], this.cartas[j]] = [this.cartas[j], this.cartas[i]];
    }
  }

  // Reparte una carta de la baraja
  repartirCarta() {
    return this.cartas.pop();
  }
}

// Creamos una clase que representa un jugador
class Jugador {
  constructor(nombre) {
    this.nombre = nombre;
    this.cartas = [];
  }

  // Roba una carta de la baraja y la añade a su mano
  robarCarta(baraja) {
    this.cartas.push(baraja.repartirCarta());
  }

  // Muestra las cartas de su mano
  mostrarCartas() {
    console.log(`Las cartas de ${this.nombre} son:`);
    for (let carta of this.cartas) {
      console.log(`${carta.numero} de ${carta.palo}`);
    }
  }

  // Calcula la puntuación de su mano
  calcularPuntuacion() {
    let puntuacion = 0;
    for (let carta of this.cartas) {
      puntuacion += carta.numero;
    }
    return puntuacion;
  }
}

// Creamos dos jugadores
const jugador1 = new Jugador("Juan");
const jugador2 = new Jugador("María");

// Creamos una baraja y la barajamos
const baraja = new Baraja();
baraja.barajar();

// Repartimos dos cartas a cada jugador
jugador1.robarCarta(baraja);
jugador1.robarCarta(baraja);
jugador2.robarCarta(baraja);
jugador2.robarCarta(baraja);

// Mostramos las cartas de cada jugador
jugador1.mostrarCartas();
jugador2.mostrarCartas();

// Calculamos la puntuación de cada jugador
const puntuacionJugador1 = jugador1.calcularPuntuacion();
const puntuacionJugador2 = jugador2.calcularPuntuacion();

// Mostramos la puntuación de cada jugador
console.log(`La puntuación de ${jugador1.nombre} es ${puntuacionJugador1}`);
console.log(`La puntuación de ${jugador2.nombre} es ${puntuacionJugador2}`);

// Comprobamos quién ha ganado
if (puntuacionJugador1 > puntuacionJugador2) {
  console.log(`${jugador1.nombre} ha ganado!`);
} else if (puntuacionJugador1 < puntuacionJugador2) {
  console.log(`${jugador2.nombre} ha ganado!`);
} else {
  console.log("Empate!");
}
```

Este código crea una baraja de cartas, baraja las cartas y reparte dos cartas a cada jugador. A continuación, calcula la puntuación de cada jugador y muestra quién ha ganado.

El código incluye varias funciones auxiliares, como `generarNumeroAleatorio()` para generar un número aleatorio entre un mínimo y un máximo, y `calcularPuntuacion()` para calcular la puntuación de una mano de cartas.

El código utiliza clases y objetos para representar la baraja, los jugadores y las cartas. Las clases se utilizan para definir los atributos y comportamientos de los objetos, y los objetos se utilizan para crear instancias de las clases.

El código también utiliza arrays para representar la baraja de cartas y las manos de los jugadores. Los arrays se utilizan para almacenar los objetos carta y jugador.

El código utiliza bucles `for` para recorrer la baraja de cartas y las manos de los jugadores. Los bucles `for` se utilizan para realizar una acción repetidamente hasta que se cumple una condición.

El código utiliza condicionales `if` para comprobar quién ha ganado el juego. Los condicionales `if` se utilizan para ejecutar un bloque de código si se cumple una condición.

El código utiliza la función `console.log()` para mostrar información en la consola. La función `console.log()` se utiliza para mostrar información en la consola del navegador.