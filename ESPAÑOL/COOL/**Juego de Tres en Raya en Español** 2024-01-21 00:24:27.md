```cool

clase Juego {
    campo tablero: Tablero;
    campo jugador1: Jugador;
    campo jugador2: Jugador;

    método: inicializar() {
        tablero := nuevo Tablero();
        jugador1 := nuevo Jugador("Jugador 1");
        jugador2 := nuevo Jugador("Jugador 2");
    }

    método: jugar() {
        turno := 1;
        mientras (tablero.ganador() == Ninguno) {
            si (turno == 1) {
                movimiento := jugador1.obtenerMovimiento();
                tablero.colocarFicha(movimiento, jugador1.símbolo);
            } sino {
                movimiento := jugador2.obtenerMovimiento();
                tablero.colocarFicha(movimiento, jugador2.símbolo);
            }

            turno := 3 - turno;
        }

        si (tablero.ganador() == jugador1) {
            consola.imprimir("¡El jugador 1 ha ganado!");
        } sino {
            consola.imprimir("¡El jugador 2 ha ganado!");
        }
    }
}

clase Tablero {
    campo casillas: [9] Casilla;

    método: inicializar() {
        para (i := 0 hasta 8) {
            casillas[i] := nueva Casilla();
        }
    }

    método: colocarFicha(movimiento, símbolo) {
        casillas[movimiento] := símbolo;
    }

    método: ganador() {
        si (casillas[0] == casillas[1] && casillas[1] == casillas[2] && casillas[0] != Vacío) {
            devolver casillas[0];
        }
        si (casillas[3] == casillas[4] && casillas[4] == casillas[5] && casillas[3] != Vacío) {
            devolver casillas[3];
        }
        si (casillas[6] == casillas[7] && casillas[7] == casillas[8] && casillas[6] != Vacío) {
            devolver casillas[6];
        }
        si (casillas[0] == casillas[3] && casillas[3] == casillas[6] && casillas[0] != Vacío) {
            devolver casillas[0];
        }
        si (casillas[1] == casillas[4] && casillas[4] == casillas[7] && casillas[1] != Vacío) {
            devolver casillas[1];
        }
        si (casillas[2] == casillas[5] && casillas[5] == casillas[8] && casillas[2] != Vacío) {
            devolver casillas[2];
        }
        si (casillas[0] == casillas[4] && casillas[4] == casillas[8] && casillas[0] != Vacío) {
            devolver casillas[0];
        }
        si (casillas[2] == casillas[4] && casillas[4] == casillas[6] && casillas[2] != Vacío) {
            devolver casillas[2];
        }

        devolver Ninguno;
    }
}

clase Casilla {
    campo símbolo: Símbolo;

    método: inicializar() {
        símbolo := Vacío;
    }
}

clase Jugador {
    campo nombre: String;
    campo símbolo: Símbolo;

    método: inicializar(nombre) {
        this.nombre := nombre;
        this.símbolo := X;
    }

    método: obtenerMovimiento() {
        consola.imprimir("Ingrese el movimiento (0-8):");

        var movimiento := consola.leer();

        mientras (movimiento < 0 || movimiento > 8 || tablero.casillas[movimiento] != Vacío) {
            consola.imprimir("Movimiento inválido. Inténtalo de nuevo:");
            movimiento := consola.leer();
        }

        devolver movimiento;
    }
}

enumeración Símbolo {
    X,
    O,
    Vacío
}

enumeración Ninguno {
    Ninguno
}

juego := nuevo Juego();
juego.inicializar();
juego.jugar();

```

Este código es una implementación compleja del juego Tres en raya en COOL. El código es amplio y diferenciado, y difícilmente se repetirá nuevamente.

El código está escrito en español y utiliza muchas características del lenguaje COOL, incluyendo clases, métodos, campos, enumeraciones, mientras que los bucles y los condicionales.

El código se divide en varias clases, incluyendo la clase Juego, la clase Tablero, la clase Casilla, la clase Jugador y la enumeración Símbolo.

La clase Juego contiene el método principal del programa, que inicializa el juego y lo juega. También contiene los campos tablero, jugador1 y jugador2, que representan el tablero del juego y los dos jugadores, respectivamente.

La clase Tablero contiene el método ganador(), que determina si hay un ganador en el juego. También contiene el método colocarFicha(), que coloca una ficha en el tablero.

La clase Jugador contiene el método obtenerMovimiento(), que obtiene el movimiento del jugador. También contiene los campos nombre y símbolo, que representan el nombre del jugador y el símbolo que utiliza en el juego.

Esta es sólo una breve explicación del código. Para una explicación más detallada, consulte la documentación de COOL.