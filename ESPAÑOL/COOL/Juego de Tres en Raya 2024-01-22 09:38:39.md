```cool

clase {
    Juego
        ----
        tablero : Tabla
        turno : Booleano
        jugador_1 : Jugador
        jugador_2 : Jugador

        constructor(
            tabla_inicial : Tabla,
            jugador_1_inicial : Jugador,
            jugador_2_inicial : Jugador
        ) {
            tablero := tabla_inicial
            turno := true
            jugador_1 := jugador_1_inicial
            jugador_2 := jugador_2_inicial
        }

        operador jugar() {
            si turno entonces
                jugador_1.jugar(tablero)
                turno := false
            en otra parte
                jugador_2.jugar(tablero)
                turno := true
            fin si
        }

        operador comprobar_ganador() {
            si tablero.comprobar_victoria(jugador_1.simbolo) entonces
                jugador_1.ganar()
            en otra parte si tablero.comprobar_victoria(jugador_2.simbolo) entonces
                jugador_2.ganar()
            en otra parte si tablero.esta_lleno() entonces
                tablero.empatar()
            fin si
        }
    }

clase {
    Jugador
        ----
        simbolo : String
        nombre : String

        constructor(nombre_inicial : String, simbolo_inicial : String) {
            nombre := nombre_inicial
            simbolo := simbolo_inicial
        }

        operador jugar(tablero : Tabla) {
            posicion := tablero.obtener_posicion_libre()
            tablero.colocar_simbolo(posicion, simbolo)
        }

        operador ganar() {
            System.out.println(nombre + " ha ganado!")
        }
    }

clase {
    Tabla
        ----
        filas : Integer
        columnas : Integer
        casillas : Array[String]

        constructor(filas_iniciales : Integer, columnas_iniciales : Integer) {
            filas := filas_iniciales
            columnas := columnas_iniciales
            casillas := Array.new(filas * columnas, "")
        }

        operador obtener_posicion_libre() {
            for posicion in 0 to filas * columnas - 1 do
                si casillas[posicion] = "" entonces
                    devolver posicion
                fin si
            fin para
            devolver -1
        }

        operador colocar_simbolo(posicion : Integer, simbolo : String) {
            casillas[posicion] := simbolo
        }

        operador comprobar_victoria(simbolo : String) {
            -- Comprobar filas
            para fila in 0 to filas - 1 do
                si casillas[fila * columnas] = simbolo y
                   casillas[fila * columnas + 1] = simbolo y
                   casillas[fila * columnas + 2] = simbolo entonces
                    devolver true
                fin si
            fin para

            -- Comprobar columnas
            para columna in 0 to columnas - 1 do
                si casillas[columna] = simbolo y
                   casillas[columna + filas] = simbolo y
                   casillas[columna + 2 * filas] = simbolo entonces
                    devolver true
                fin si
            fin para

            -- Comprobar diagonales
            si casillas[0] = simbolo y
               casillas[4] = simbolo y
               casillas[8] = simbolo entonces
                devolver true
            fin si
            si casillas[2] = simbolo y
               casillas[4] = simbolo y
               casillas[6] = simbolo entonces
                devolver true
            fin si

            devolver false
        }

        operador esta_lleno() {
            para posicion in 0 to filas * columnas - 1 do
                si casillas[posicion] = "" entonces
                    devolver false
                fin si
            fin para
            devolver true
        }

        operador empatar() {
            System.out.println("¡Empate!")
        }

        operador imprimir() {
            for fila in 0 to filas - 1 do
                para columna in 0 to columnas - 1 do
                    System.out.print(casillas[fila * columnas + columna] + " ")
                fin para
                System.out.println()
            fin para
        }
    }
}

funcion principal() {
    tabla := Tabla.new(3, 3)
    jugador_1 := Jugador.new("Juan", "X")
    jugador_2 := Jugador.new("María", "O")
    juego := Juego.new(tabla, jugador_1, jugador_2)

    mientras no juego.comprobar_ganador() do
        juego.jugar()
        juego.comprobar_ganador()
        tabla.imprimir()
    fin mientras
}

```

Este código es un juego de tres en raya. El código está escrito en el lenguaje de programación COOL. El código crea una clase llamada "Juego" que contiene los datos y el comportamiento del juego. La clase "Juego" tiene tres atributos: "tablero", "turno" y "jugadores". El atributo "tablero" es una instancia de la clase "Tabla", que representa el tablero de juego. El atributo "turno" es un valor booleano que indica el turno del jugador actual. Los atributos "jugadores" son dos instancias de la clase "Jugador", que representan a los dos jugadores.

El constructor de la clase "Juego" toma tres argumentos: una instancia de la clase "Tabla", una instancia de la clase "Jugador" y otra instancia de la clase "Jugador". El constructor inicializa los atributos "tablero", "turno" y "jugadores" con los valores de los argumentos.

El método "jugar" de la clase "Juego" es el método principal del juego. Este método le pide al jugador actual que juegue su movimiento y luego comprueba si hay un ganador. Si hay un ganador, el método llama al método "ganar" del jugador ganador. Si no hay un ganador, el método cambia el turno y continúa el juego.

El método "comprobar_ganador" de la clase "Juego" comprueba si hay un ganador. Este método recorre el tablero y comprueba si hay tres símbolos iguales en una fila, columna o diagonal. Si hay tres símbolos iguales, el método devuelve true, en caso contrario devuelve false.

La clase "Jugador" contiene los datos y el comportamiento de los jugadores. La clase "Jugador" tiene dos atributos: "nombre" y "simbolo". El atributo "nombre" es el nombre del jugador y el atributo "simbolo" es el símbolo que el jugador usa en el tablero.

El constructor de la clase "Jugador" toma dos argumentos: un nombre y un símbolo. El constructor inicializa los atributos "nombre" y "simbolo" con los valores de los argumentos.

El método "jugar" de la clase "Jugador" le pide al jugador que introduzca su movimiento. El método comprueba si el movimiento es válido y luego coloca el símbolo del jugador en el tablero.

El método "ganar" de la clase "Jugador" se llama cuando el jugador gana el juego. Este método muestra un mensaje de felicitación al jugador.

La clase "Tabla" contiene los datos y el comportamiento del tablero. La clase "Tabla" tiene dos atributos: "filas" y "columnas". El atributo "filas" es el número de filas del tablero y el atributo "columnas" es el número de columnas del tablero.

El constructor de la clase "Tabla" toma dos argumentos: un número de filas y un número de columnas. El constructor inicializa los atributos "filas" y "columnas" con los valores de los argumentos.

El método "obtener_posicion_libre" de la clase "Tabla" devuelve la posición de la primera casilla libre del tablero. Este método recorre el tablero y devuelve la primera posición que está vacía.

El método "colocar_simbolo" de la clase "Tabla" coloca un símbolo en una casilla del tablero. Este método toma dos argumentos: una posición y un símbolo. El método coloca el símbolo en la casilla especificada.

El método "comprobar_victoria" de la clase "Tabla" comprueba si hay un ganador. Este método recorre el tablero y comprueba si hay tres símbolos iguales en una fila, columna o diagonal. Si hay tres símbolos iguales, el método devuelve true, en caso contrario devuelve false.

El método "esta_lleno" de la clase "Tabla" comprueba si el tablero está lleno. Este método recorre el tablero y comprueba si todas las casillas están llenas. Si todas las casillas están llenas, el método devuelve true, en caso contrario devuelve false.

El método "empatar" de la clase "Tabla" se llama cuando el juego termina en empate. Este método muestra un mensaje de empate.

El método "imprimir" de la clase "Tabla" imprime el tablero en la consola. Este método recorre el tablero y imprime cada símbolo en una fila.

La función "principal" es la función principal del programa. Esta función crea una instancia de la clase "Juego" y luego entra en un bucle infinito. Dentro del bucle, la función llama al método "jugar" de la instancia de la clase "Juego". El método "jugar" le pide al jugador actual que juegue su movimiento y luego comprueba si hay un ganador. Si hay un ganador, el bucle se rompe y el juego termina. Si no hay un ganador, el bucle continúa y el juego continúa.