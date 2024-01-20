```lua
-- Definimos una función para crear un nuevo jugador.
function crearJugador(nombre, vida, ataque, defensa)
  -- Creamos una tabla para almacenar los datos del jugador.
  local jugador = {}

  -- Almacenamos los datos del jugador en la tabla.
  jugador.nombre = nombre
  jugador.vida = vida
  jugador.ataque = ataque
  jugador.defensa = defensa

  -- Devolvemos la tabla con los datos del jugador.
  return jugador
end

-- Creamos un nuevo jugador llamado "Jhon".
local jhon = crearJugador("Jhon", 100, 10, 5)

-- Creamos un nuevo jugador llamado "Maria".
local maria = crearJugador("Maria", 120, 8, 6)

-- Creamos una función para iniciar una batalla entre dos jugadores.
function iniciarBatalla(jugador1, jugador2)
  -- Creamos un bucle que se repetirá mientras los dos jugadores tengan vida.
  while jugador1.vida > 0 and jugador2.vida > 0 do
    -- Calculamos el daño que inflige el jugador1 al jugador2.
    local daño = jugador1.ataque - jugador2.defensa

    -- Si el daño es negativo, lo convertimos en 0.
    if daño < 0 then
      daño = 0
    end

    -- Reducimos la vida del jugador2 en función del daño recibido.
    jugador2.vida = jugador2.vida - daño

    -- Calculamos el daño que inflige el jugador2 al jugador1.
    daño = jugador2.ataque - jugador1.defensa

    -- Si el daño es negativo, lo convertimos en 0.
    if daño < 0 then
      daño = 0
    end

    -- Reducimos la vida del jugador1 en función del daño recibido.
    jugador1.vida = jugador1.vida - daño

    -- Imprimimos el estado de la batalla en la consola.
    print("Jhon tiene " .. jhon.vida .. " de vida.")
    print("Maria tiene " .. maria.vida .. " de vida.")
  end

  -- Si el jugador1 tiene más vida que el jugador2, entonces el jugador1 gana la batalla.
  if jugador1.vida > jugador2.vida then
    print("Jhon gana la batalla!")
  -- Si el jugador2 tiene más vida que el jugador1, entonces el jugador2 gana la batalla.
  elseif jugador2.vida > jugador1.vida then
    print("Maria gana la batalla!")
  -- Si los dos jugadores tienen la misma vida, entonces la batalla termina en empate.
  else
    print("La batalla termina en empate!")
  end
end

-- Iniciamos una batalla entre Jhon y Maria.
iniciarBatalla(jhon, maria)
```

Este código crea dos jugadores, Jhon y Maria, con diferentes valores de vida, ataque y defensa. Luego, define una función para iniciar una batalla entre dos jugadores, que calcula el daño que cada jugador inflige al otro y reduce la vida del jugador que recibe el daño. Finalmente, inicia una batalla entre Jhon y Maria y muestra el resultado de la batalla en la consola.