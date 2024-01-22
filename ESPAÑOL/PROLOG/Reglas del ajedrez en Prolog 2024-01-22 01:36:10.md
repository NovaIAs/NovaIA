```prolog
% Definimos las reglas del juego de ajedrez.

% Para cada tipo de pieza, definimos sus posibles movimientos.

pieza(peon, [{-1, 0}, {-1, -1}, {-1, 1}]).
pieza(torre, [[0, D] | D1]) :- between(1, 8, D), D1 = [[0, -D] | [[D, 0] | [[D, D] | [[D, -D] | []]]].
pieza(caballo, [[-1, -2], [-1, 2], [1, -2], [1, 2], [-2, -1], [-2, 1], [2, -1], [2, 1]]).
pieza(alfil, [[D, D] | D1]) :- between(1, 8, D), D1 = [[D, -D] | [[D, 0] | [[D, D] | []]]].
pieza(reina, [[D, D] | D1]) :- between(1, 8, D), D1 = [[D, -D] | [[D, 0] | [[D, D] | [[D, -D] | []]]].
pieza(rey, [{-1, 0}, {0, -1}, {1, 0}, {0, 1}, {-1, 1}, {1, 1}, {-1, -1}, {1, -1}]).

% Definimos la función para comprobar si una posición es válida.

posicion_valida(Posicion) :-
  between(1, 8, PosicionX),
  between(1, 8, PosicionY).

% Definimos la función para comprobar si una casilla está vacía.

casilla_vacia(Tablero, Posicion) :-
  nth1(PosicionX, Tablero, Fila),
  nth1(PosicionY, Fila, Casilla),
  Casilla = '-'.

% Definimos la función para comprobar si una casilla está ocupada por una pieza del mismo color.

casilla_ocupada_mismo_color(Tablero, Posicion, Color) :-
  nth1(PosicionX, Tablero, Fila),
  nth1(PosicionY, Fila, Casilla),
  Casilla \= '-',
  pieza(Casilla, _),
  color(Casilla, Color).

% Definimos la función para comprobar si una casilla está ocupada por una pieza del color contrario.

casilla_ocupada_color_contrario(Tablero, Posicion, Color) :-
  nth1(PosicionX, Tablero, Fila),
  nth1(PosicionY, Fila, Casilla),
  Casilla \= '-',
  pieza(Casilla, _),
  not(color(Casilla, Color)).

% Definimos la función para mover una pieza.

mover(Tablero, PosicionOrigen, PosicionDestino, Color) :-
  posicion_valida(PosicionOrigen),
  posicion_valida(PosicionDestino),
  casilla_ocupada_mismo_color(Tablero, PosicionOrigen, Color),
  casilla_vacia(Tablero, PosicionDestino),
  pieza(Pieza, Movimientos),
  nth1(PosXOrigen, Tablero, FilaOrigen),
  nth1(PosYOrigen, FilaOrigen, PiezaOrigen),
  nth1(PosXDestino, Tablero, FilaDestino),
  remplazar(PiezaOrigen, '-', FilaOrigen),
  remplazar('-', Pieza, FilaDestino),
  nth1(PosYDestino, FilaDestino, PiezaDestino),
  member(Movimiento, Movimientos),
  PosX es PosXOrigen + MovimientoX,
  PosY es PosYOrigen + MovimientoY,
  PosicionDestino = {PosX, PosY},
  not(casilla_ocupada_mismo_color(Tablero, PosicionDestino, Color)).

% Definimos la función para reemplazar un elemento en una lista.

remplazar(ElementoViejo, ElementoNuevo, Lista) :-
  nth1(Posicion, Lista, ElementoViejo),
  nth1(Posicion, Lista, ElementoNuevo).

% Definimos la función para comprobar si un jugador está en jaque.

en_jaque(Tablero, Color) :-
  posicion_valida(PosicionRey),
  casilla_ocupada_mismo_color(Tablero, PosicionRey, Color),
  pieza(Rey, MovimientosRey),
  nth1(PosXRey, Tablero, FilaRey),
  nth1(PosYRey, FilaRey, Rey),
  forall(member(Movimiento, MovimientosRey),
    PosX es PosXRey + MovimientoX,
    PosY es PosYRey + MovimientoY,
    PosicionAtaque = {PosX, PosY},
    casilla_ocupada_color_contrario(Tablero, PosicionAtaque, Color)).

% Definimos la función para comprobar si un jugador está en jaque mate.

jaque_mate(Tablero, Color) :-
  en_jaque(Tablero, Color),
  forall(
    posicion_valida(PosicionOrigen),
    casilla_ocup