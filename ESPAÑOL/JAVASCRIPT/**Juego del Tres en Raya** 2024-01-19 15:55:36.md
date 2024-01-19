```javascript
// Definimos las variables globales
let matriz = []; // Matriz que contendrá los valores del tablero
let jugadorActual = 1; // Jugador actual (1 o 2)
let finalJuego = false; // Booleano que indica si el juego ha terminado

// Función para crear el tablero
const crearTablero = () => {
  // Creamos una matriz de 3x3
  for (let i = 0; i < 3; i++) {
    matriz.push([]);
    for (let j = 0; j < 3; j++) {
      // Inicializamos todas las posiciones con un valor vacío
      matriz[i].push("");
    }
  }
};

// Función para mostrar el tablero en la consola
const mostrarTablero = () => {
  // Recorremos la matriz y mostramos los valores en la consola
  for (let i = 0; i < matriz.length; i++) {
    console.log(matriz[i].join(" | "));
  }
};

// Función para realizar una jugada
const realizarJugada = (fila, columna) => {
  // Comprobamos si la posición está vacía
  if (matriz[fila][columna] === "") {
    // Si está vacía, la marcamos con el símbolo del jugador actual
    matriz[fila][columna] = jugadorActual === 1 ? "X" : "O";

    // Cambiamos el turno al otro jugador
    jugadorActual = jugadorActual === 1 ? 2 : 1;

    // Comprobamos si hay un ganador
    if (comprobarGanador()) {
      // Si hay un ganador, mostramos un mensaje y damos por terminado el juego
      console.log(`El jugador ${jugadorActual === 1 ? "X" : "O"} ha ganado!`);
      finalJuego = true;
    } else if (comprobarEmpate()) {
      // Si hay un empate, mostramos un mensaje y damos por terminado el juego
      console.log("Empate!");
      finalJuego = true;
    }
  } else {
    // Si la posición no está vacía, mostramos un mensaje de error
    console.log("Posición ocupada. Inténtalo de nuevo.");
  }
};

// Función para comprobar si hay un ganador
const comprobarGanador = () => {
  // Recorremos la matriz y comprobamos si hay alguna fila, columna o diagonal con tres símbolos iguales
  for (let i = 0; i < matriz.length; i++) {
    // Comprobamos las filas
    if (
      matriz[i][0] !== "" &&
      matriz[i][0] === matriz[i][1] &&
      matriz[i][1] === matriz[i][2]
    ) {
      return true;
    }

    // Comprobamos las columnas
    if (
      matriz[0][i] !== "" &&
      matriz[0][i] === matriz[1][i] &&
      matriz[1][i] === matriz[2][i]
    ) {
      return true;
    }
  }

  // Comprobamos las diagonales
  if (
    matriz[0][0] !== "" &&
    matriz[0][0] === matriz[1][1] &&
    matriz[1][1] === matriz[2][2]
  ) {
    return true;
  }

  if (
    matriz[2][0] !== "" &&
    matriz[2][0] === matriz[1][1] &&
    matriz[1][1] === matriz[0][2]
  ) {
    return true;
  }

  // Si no hay ningún ganador, devolvemos false
  return false;
};

// Función para comprobar si hay un empate
const comprobarEmpate = () => {
  // Recorremos la matriz y comprobamos si todas las posiciones están ocupadas
  for (let i = 0; i < matriz.length; i++) {
    for (let j = 0; j < matriz[i].length; j++) {
      if (matriz[i][j] === "") {
        return false; // Si hay alguna posición vacía, no hay empate
      }
    }
  }

  // Si todas las posiciones están ocupadas, hay empate
  return true;
};

// Llamamos a la función crearTablero para crear el tablero
crearTablero();

// Mostramos el tablero en la consola
mostrarTablero();

// Añadimos un evento 'click' a los elementos con la clase 'celda'
const celdas = document.querySelectorAll(".celda");
celdas.forEach((celda) => {
  celda.addEventListener("click", () => {
    // Obtenemos la fila y la columna de la celda
    const fila = parseInt(celda.dataset.fila);
    const columna = parseInt(celda.dataset.columna);

    // Realizamos la jugada
    realizarJugada(fila, columna);

    // Mostramos el tablero en la consola
    mostrarTablero();

    // Comprobamos si el juego ha terminado
    if (finalJuego) {
      // Si el juego ha terminado, desactivamos los eventos 'click' de las celdas
      celdas.forEach((celda) => {
        celda.removeEventListener("click", () => {});
      });
    }
  });
});
```

Explicación del código:

* La función `crearTablero()` crea una matriz de 3x3, que representa el tablero.


* La función `mostrarTablero()` muestra el tablero en la consola.


* La función `realizarJugada()` recibe la fila y la columna de la celda seleccionada y marca la posición correspondiente en el tablero con el símbolo del jugador actual.


* La función `comprobarGanador()` recorre el tablero y comprueba si hay alguna fila, columna o diagonal con tres símbolos iguales.


* La función `comprobarEmpate()` recorre el tablero y comprueba si todas las posiciones están ocupadas.


* Añadimos un evento 'click' a los elementos con la clase 'celda' para que cuando el usuario haga clic en una celda, se realice la jugada correspondiente.


* Comprobamos si el juego ha terminado después de cada jugada y, si es así, mostramos un mensaje y desactivamos los eventos 'click' de las celdas.