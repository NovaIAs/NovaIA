```lua
-- Este código es un ejemplo avanzado de programación en LUA que simula un juego de ajedrez.

-- Definimos las piezas del ajedrez como constantes.
local PEON = "Peón"
local CABALLO = "Caballo"
local ALFIL = "Alfil"
local TORRE = "Torre"
local REINA = "Reina"
local REY = "Rey"

-- Creamos un tablero de ajedrez vacío.
local tablero = {}
for i = 1, 8 do
  tablero[i] = {}
  for j = 1, 8 do
    tablero[i][j] = nil
  end
end

-- Colocamos las piezas iniciales en el tablero.
tablero[1][1] = TORRE
tablero[1][2] = CABALLO
tablero[1][3] = ALFIL
tablero[1][4] = REINA
tablero[1][5] = REY
tablero[1][6] = ALFIL
tablero[1][7] = CABALLO
tablero[1][8] = TORRE
for i = 1, 8 do
  tablero[2][i] = PEON
end

-- Definimos una función para mover una pieza.
function mover(pieza, fila, columna)
  if tablero[fila][columna] == nil then
    tablero[fila][columna] = pieza
    tablero[pieza.fila][pieza.columna] = nil
    pieza.fila = fila
    pieza.columna = columna
  end
end

-- Definimos una función para capturar una pieza.
function capturar(pieza)
  tablero[pieza.fila][pieza.columna] = nil
end

-- Definimos una función para calcular los movimientos posibles de una pieza.
function movimientos_posibles(pieza)
  local movimientos = {}
  if pieza.tipo == PEON then
    if pieza.color == "blanco" then
      if tablero[pieza.fila - 1][pieza.columna] == nil then
        movimientos[#movimientos + 1] = {pieza.fila - 1, pieza.columna}
        if pieza.fila == 2 and tablero[pieza.fila - 2][pieza.columna] == nil then
          movimientos[#movimientos + 1] = {pieza.fila - 2, pieza.columna}
        end
      end
      if pieza.columna > 1 and tablero[pieza.fila - 1][pieza.columna - 1] ~= nil and tablero[pieza.fila - 1][pieza.columna - 1].color ~= pieza.color then
        movimientos[#movimientos + 1] = {pieza.fila - 1, pieza.columna - 1}
      end
      if pieza.columna < 8 and tablero[pieza.fila - 1][pieza.columna + 1] ~= nil and tablero[pieza.fila - 1][pieza.columna + 1].color ~= pieza.color then
        movimientos[#movimientos + 1] = {pieza.fila - 1, pieza.columna + 1}
      end
    else
      if tablero[pieza.fila + 1][pieza.columna] == nil then
        movimientos[#movimientos + 1] = {pieza.fila + 1, pieza.columna}
        if pieza.fila == 7 and tablero[pieza.fila + 2][pieza.columna] == nil then
          movimientos[#movimientos + 1] = {pieza.fila + 2, pieza.columna}
        end
      end
      if pieza.columna > 1 and tablero[pieza.fila + 1][pieza.columna - 1] ~= nil and tablero[pieza.fila + 1][pieza.columna - 1].color ~= pieza.color then
        movimientos[#movimientos + 1] = {pieza.fila + 1, pieza.columna - 1}
      end
      if pieza.columna < 8 and tablero[pieza.fila + 1][pieza.columna + 1] ~= nil and tablero[pieza.fila + 1][pieza.columna + 1].color ~= pieza.color then
        movimientos[#movimientos +