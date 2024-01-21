```
-- Variables globales
local jugadores = {}

-- Definición de la clase Jugador
local Jugador = {}
Jugador.new = function(this, nombre, puntos)
  this.nombre = nombre
  this.puntos = puntos
end
Jugador.saludar = function(this)
  print("Hola, mi nombre es " .. this.nombre .. " y tengo " .. this.puntos .. " puntos.")
end

-- Función para añadir un jugador a la lista de jugadores
function añadirJugador(nombre, puntos)
  local jugador = Jugador.new(nombre, puntos)
  table.insert(jugadores, jugador)
end

-- Función para mostrar todos los jugadores
function mostrarJugadores()
  for _, jugador in ipairs(jugadores) do
    jugador.saludar()
  end
end

-- Función principal
function main()
  -- Añade algunos jugadores a la lista
  añadirJugador("Juan", 100)
  añadirJugador("María", 200)
  añadirJugador("Pedro", 300)

  -- Muestra todos los jugadores
  mostrarJugadores()
end

-- Llama a la función principal
main()
```

Explicación del código:

* Se define una clase `Jugador` que tiene dos atributos, `nombre` y `puntos`.
* Se define una función `añadirJugador` que añade un nuevo jugador a la lista de jugadores.
* Se define una función `mostrarJugadores` que muestra todos los jugadores de la lista.
* En la función `main`, se añaden algunos jugadores a la lista y luego se muestran todos ellos.