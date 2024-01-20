```prolog

% Reglas para el juego del ajedrez.

% Representar una posición en el tablero de ajedrez.

tablero(
  [[t,c,a,r,d,a,c,t],
   [p,p,p,p,p,p,p,p],
   [.,.,.,.,.,.,.,.],
   [.,.,.,.,.,.,.,.],
   [.,.,.,.,.,.,.,.],
   [.,.,.,.,.,.,.,.],
   [P,P,P,P,P,P,P,P],
   [T,C,A,R,D,A,C,T]]
).

% Representar un movimiento en el tablero de ajedrez.

movimiento(Origen, Destino) :-
  tablero(T),
  nth0(OrigenY, T, OrigenFila),
  nth0(OrigenX, OrigenFila, OrigenPieza),
  nth0(DestinoY, T, DestinoFila),
  nth0(DestinoX, DestinoFila, DestinoPieza),
  OrigenPieza \= .,
  DestinoPieza = .,
  es_movimiento_valido(OrigenPieza, OrigenX, OrigenY, DestinoX, DestinoY).

% Comprobar si un movimiento es válido.

es_movimiento_valido(Pieza, OrigenX, OrigenY, DestinoX, DestinoY) :-
  es_movimiento_peon(Pieza, OrigenX, OrigenY, DestinoX, DestinoY).

% Comprobar si un movimiento es válido para un peón.

es_movimiento_peon(Pieza, OrigenX, OrigenY, DestinoX, DestinoY) :-
  Pieza = p,
  OrigenY + 1 = DestinoY,
  OrigenX = DestinoX.

% Comprobar si un movimiento es válido para un caballo.

es_movimiento_caballo(Pieza, OrigenX, OrigenY, DestinoX, DestinoY) :-
  Pieza = c,
  abs(OrigenX - DestinoX) = 2,
  abs(OrigenY - DestinoY) = 1.

% Comprobar si un movimiento es válido para una torre.

es_movimiento_torre(Pieza, OrigenX, OrigenY, DestinoX, DestinoY) :-
  Pieza = t,
  OrigenX = DestinoX,
  OrigenY \= DestinoY.

% Comprobar si un movimiento es válido para un alfil.

es_movimiento_alfil(Pieza, OrigenX, OrigenY, DestinoX, DestinoY) :-
  Pieza = a,
  abs(OrigenX - DestinoX) = abs(OrigenY - DestinoY).

% Comprobar si un movimiento es válido para una reina.

es_movimiento_reina(Pieza, OrigenX, OrigenY, DestinoX, DestinoY) :-
  Pieza = d,
  (OrigenX = DestinoX, OrigenY \= DestinoY)
  ;(OrigenX \= DestinoX, OrigenY = DestinoY)
  ;(abs(OrigenX - DestinoX) = abs(OrigenY - DestinoY)).

% Comprobar si un movimiento es válido para un rey.

es_movimiento_rey(Pieza, OrigenX, OrigenY, DestinoX, DestinoY) :-
  Pieza = r,
  abs(OrigenX - DestinoX) =< 1,
  abs(OrigenY - DestinoY) =< 1.

% Comprobar si un movimiento es válido para una pieza.

es_movimiento_valido(Pieza, OrigenX, OrigenY, DestinoX, DestinoY) :-
  es_movimiento_peon(Pieza, OrigenX, OrigenY, DestinoX, DestinoY)
  ;(es_movimiento_caballo(Pieza, OrigenX, OrigenY, DestinoX, DestinoY))
  ;(es_movimiento_torre(Pieza, OrigenX, OrigenY, DestinoX, DestinoY))
  ;(es_movimiento_alfil(Pieza, OrigenX, OrigenY, DestinoX, DestinoY))
  ;(es_movimiento_reina(Pieza, OrigenX, OrigenY, DestinoX, DestinoY))
  ;(es_movimiento_rey(Pieza, OrigenX, Origen