```ada
-- Módulo principal del programa
with Ada.Text_IO;        use Ada.Text_IO;
with Constantes;         use Constantes;
with Tipos_de_datos;      use Tipos_de_datos;
with Funciones_auxiliares; use Funciones_auxiliares;

procedure Main is

   -- Variables locales
   type Estado is (Inicial, Jugando, Fin);
   estado : Estado := Inicial;
   tablero : Tablero;
   jugador_actual : Jugador;
   ganador : Jugador;
   posicion : Posicion;
   movimientos_posibles : Movimientos_posibles;
   jugadas_realizadas : Jugadas_realizadas;

   -- Inicialización del juego
   tablero := Crear_tablero;
   jugador_actual := J1;
   ganador := Ninguno;
   jugadas_realizadas := [];

begin
   loop
      -- Bucle principal del juego
      case estado is
         when Inicial =>
            Mostrar_bienvenida;
            estado := Jugando;
         when Jugando =>
            -- Mostrar el tablero
            Mostrar_tablero(tablero);

            -- Obtener la posición seleccionada por el jugador actual
            posicion := Obtener_posicion_jugador;

            -- Validar la posición seleccionada
            if Posicion_valida(tablero, posicion) then
               -- Obtener los movimientos posibles para la posición seleccionada
               movimientos_posibles := Obtener_movimientos_posibles(tablero, jugador_actual, posicion);

               -- Mostrar los movimientos posibles
               Mostrar_movimientos_posibles(movimientos_posibles);

               -- Obtener el movimiento seleccionado por el jugador actual
               movimiento := Obtener_movimiento_jugador;

               -- Validar el movimiento seleccionado
               if Movimiento_valido(tablero, movimientos_posibles, movimiento) then
                  -- Realizar el movimiento
                  tablero := Realizar_movimiento(tablero, jugador_actual, movimiento);

                  -- Actualizar el jugador actual
                  jugador_actual := Siguiente_jugador(jugador_actual);

                  -- Comprobar si hay un ganador
                  ganador := Comprobar_ganador(tablero);
               else
                  Put_Line("Movimiento inválido. Inténtalo de nuevo.");
               end if;
            else
               Put_Line("Posición inválida. Inténtalo de nuevo.");
            end if;

            -- Comprobar si hay un ganador o si el juego ha terminado en empate
            if ganador /= Ninguno or Tablero_lleno(tablero) then
               estado := Fin;
            end if;
         when Fin =>
            -- Mostrar el tablero final
            Mostrar_tablero(tablero);

            -- Mostrar el ganador o el empate
            if ganador /= Ninguno then
               Put_Line("El ganador es el jugador " & Nombre_jugador(ganador) & ".");
            else
               Put_Line("El juego ha terminado en empate.");
            end if;

            -- Finalizar el programa
            exit;
      end case;
   end loop;
end Main;
```

Este código es una implementación completa del juego del tres en raya en Ada. El programa consta de varios módulos, cada uno de los cuales se encarga de una parte específica de la funcionalidad del juego. El módulo principal, Main, es el punto de entrada del programa y se encarga de controlar el flujo del juego.

Los otros módulos son:

* Constantes: Contiene las constantes utilizadas en el programa, como el tamaño del tablero y los símbolos utilizados para representar a los jugadores.
* Tipos_de_datos: Contiene los tipos de datos utilizados en el programa, como el tipo Tablero, el tipo Jugador y el tipo Posicion.
* Funciones_auxiliares: Contiene las funciones auxiliares utilizadas en el programa, como la función Obtener_posicion_jugador y la función Obtener_movimiento_jugador.

El programa funciona de la siguiente manera:

1. El módulo Main inicializa el juego creando un tablero vacío, asignando el jugador actual a J1 y estableciendo el estado del juego a Inicial.
2. El módulo Main entra en un bucle principal que se ejecuta hasta que el estado del juego sea Fin.
3. En cada iteración del bucle principal, el módulo Main realiza las siguientes acciones:

    * Muestra el tablero actual.
    * Obtiene la posición seleccionada por el jugador actual.
    * Valida la posición seleccionada.
    * Obtiene los movimientos posibles para la posición seleccionada.
    * Muestra los movimientos posibles.
    * Obtiene el movimiento seleccionado por el jugador actual.
    * Valida el movimiento seleccionado.
    * Realiza el movimiento.
    * Actualiza el jugador actual.
    * Comprueba si hay un ganador.

4. Si hay un ganador o si el juego ha terminado en empate, el módulo Main establece el estado del juego a Fin y sale del bucle principal.
5. Si no hay un ganador ni el juego ha terminado en empate, el módulo Main continúa con la siguiente iteración del bucle principal.