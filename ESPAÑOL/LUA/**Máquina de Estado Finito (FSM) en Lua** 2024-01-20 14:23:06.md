```lua
-- Este código es una demostración de una máquina de estado finito (FSM) simple en Lua.
-- Una FSM es un modelo matemático de un sistema que puede estar en uno de un conjunto finito de estados.
-- El sistema puede pasar de un estado a otro en respuesta a eventos.

-- Definimos los estados de la FSM. En este caso, los estados son "A", "B" y "C".
local estados = {"A", "B", "C"}

-- Definimos los eventos que pueden ocurrir en la FSM. En este caso, los eventos son "e1", "e2" y "e3".
local eventos = {"e1", "e2", "e3"}

-- Definimos la tabla de transición de estados. Esta tabla define cómo cambia el estado de la FSM en respuesta a un evento.
-- Por ejemplo, si la FSM está en el estado "A" y ocurre el evento "e1", la FSM pasará al estado "B".
local tablaTransicionEstados = {
    ["A"] = {
        ["e1"] = "B",
        ["e2"] = "C",
        ["e3"] = "A",
    },
    ["B"] = {
        ["e1"] = "C",
        ["e2"] = "A",
        ["e3"] = "B",
    },
    ["C"] = {
        ["e1"] = "A",
        ["e2"] = "B",
        ["e3"] = "C",
    },
}

-- Definimos la función que ejecuta la FSM. Esta función toma como parámetros el estado actual de la FSM y el evento que ha ocurrido.
-- La función devuelve el nuevo estado de la FSM.
function ejecutarFSM(estadoActual, evento)
    local nuevoEstado = tablaTransicionEstados[estadoActual][evento]
    return nuevoEstado
end

-- Creamos un bucle que ejecuta la FSM hasta que el usuario ingrese la letra "q" para salir.
while true do
    -- Mostramos el estado actual de la FSM.
    print("Estado actual:", estadoActual)

    -- Pedimos al usuario que ingrese un evento.
    print("Ingrese un evento (e1, e2, e3, q para salir):")
    local evento = io.read()

    -- Si el usuario ingresó la letra "q", salimos del bucle.
    if evento == "q" then
        break
    end

    -- Ejecutamos la FSM con el estado actual y el evento ingresado por el usuario.
    estadoActual = ejecutarFSM(estadoActual, evento)
end
```

Explicación del código:

* La función `ejecutarFSM` toma como parámetros el estado actual de la FSM y el evento que ha ocurrido. La función devuelve el nuevo estado de la FSM.
* La tabla `tablaTransicionEstados` define cómo cambia el estado de la FSM en respuesta a un evento. Por ejemplo, si la FSM está en el estado "A" y ocurre el evento "e1", la FSM pasará al estado "B".
* El bucle `while` ejecuta la FSM hasta que el usuario ingrese la letra "q" para salir.
* En cada iteración del bucle, se muestra el estado actual de la FSM y se pide al usuario que ingrese un evento.
* Si el usuario ingresa la letra "q", se sale del bucle.
* De lo contrario, se ejecuta la FSM con el estado actual y el evento ingresado por el usuario.