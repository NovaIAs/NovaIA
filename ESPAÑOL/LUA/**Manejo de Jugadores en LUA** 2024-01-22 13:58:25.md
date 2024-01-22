```lua
-- Crear una tabla de jugadores
jugadores = {}

-- Añadir jugadores a la tabla
jugadores["Juan"] = {
    nombre = "Juan",
    edad = 25,
    puntuación = 0
}
jugadores["María"] = {
    nombre = "María",
    edad = 30,
    puntuación = 0
}
jugadores["Pedro"] = {
    nombre = "Pedro",
    edad = 35,
    puntuación = 0
}

-- Crear una función para mostrar la tabla de jugadores
function mostrarJugadores()
    print("Jugadores:")
    for nombre, jugador in pairs(jugadores) do
        print(string.format("%s (%d años, puntuación: %d)", jugador.nombre, jugador.edad, jugador.puntuación))
    end
end

-- Crear una función para añadir un nuevo jugador a la tabla
function añadirJugador(nombre, edad)
    jugadores[nombre] = {
        nombre = nombre,
        edad = edad,
        puntuación = 0
    }
end

-- Crear una función para eliminar un jugador de la tabla
function eliminarJugador(nombre)
    jugadores[nombre] = nil
end

-- Crear una función para actualizar la puntuación de un jugador
function actualizarPuntuación(nombre, puntuación)
    jugadores[nombre].puntuación = puntuación
end

-- Mostrar la tabla de jugadores inicial
mostrarJugadores()

-- Añadir un nuevo jugador
añadirJugador("Ana", 20)

-- Eliminar un jugador
eliminarJugador("Pedro")

-- Actualizar la puntuación de un jugador
actualizarPuntuación("María", 100)

-- Mostrar la tabla de jugadores final
mostrarJugadores()
```

Este código crea una tabla de jugadores, con sus nombres, edades y puntuaciones. Luego, se definen varias funciones para añadir, eliminar y actualizar jugadores, así como para mostrar la tabla de jugadores. Finalmente, se llama a estas funciones para añadir, eliminar y actualizar jugadores, y para mostrar la tabla de jugadores final.

El código utiliza varias características de LUA, como tablas, funciones y bucles, para crear un programa complejo y funcional.