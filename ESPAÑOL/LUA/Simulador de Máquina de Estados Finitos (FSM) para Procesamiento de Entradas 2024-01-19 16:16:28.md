```lua
-- Este código simula una máquina de estados finitos (FSM) en Lua.
-- Una FSM es un tipo de autómata que puede estar en un número finito de estados.
-- En cada estado, la FSM puede recibir una entrada y, en función de esa entrada,
-- puede cambiar de estado y realizar una o más acciones.

-- Definimos los estados de la FSM.
estados = {
  "estado_inicial",
  "estado_1",
  "estado_2",
  "estado_final"
}

-- Definimos las entradas que puede recibir la FSM.
entradas = {
  "entrada_1",
  "entrada_2",
  "entrada_3"
}

-- Definimos la tabla de transición de estados.
tabla_transicion = {
  { "estado_inicial", "entrada_1", "estado_1" },
  { "estado_1", "entrada_2", "estado_2" },
  { "estado_2", "entrada_3", "estado_final" }
}

-- Definimos la tabla de acciones.
tabla_acciones = {
  { "estado_inicial", "entrada_1", "Acción 1" },
  { "estado_1", "entrada_2", "Acción 2" },
  { "estado_2", "entrada_3", "Acción 3" }
}

-- Definimos el estado inicial de la FSM.
estado_actual = "estado_inicial"

-- Definimos una función para procesar una entrada.
function procesar_entrada(entrada)
  -- Buscamos la transición de estado correspondiente a la entrada actual.
  for i = 1, #tabla_transicion do
    if tabla_transicion[i][1] == estado_actual and tabla_transicion[i][2] == entrada then
      -- Si encontramos la transición, cambiamos de estado y realizamos la acción correspondiente.
      estado_actual = tabla_transicion[i][3]
      print(tabla_acciones[i][3])
      break
    end
  end
end

-- Procesamos una serie de entradas.
entradas_a_procesar = {"entrada_1", "entrada_2", "entrada_3"}
for i = 1, #entradas_a_procesar do
  procesar_entrada(entradas_a_procesar[i])
end
```

Este código simula una FSM que tiene tres estados: "estado_inicial", "estado_1" y "estado_final". La FSM puede recibir tres entradas: "entrada_1", "entrada_2" y "entrada_3".

La tabla de transición de estados define cómo la FSM cambia de estado en función de la entrada actual. Por ejemplo, si la FSM está en el estado "estado_inicial" y recibe la entrada "entrada_1", entonces cambia al estado "estado_1".

La tabla de acciones define qué acciones se realizan cuando la FSM cambia de estado. Por ejemplo, si la FSM cambia del estado "estado_inicial" al estado "estado_1", entonces se realiza la "Acción 1".

El código comienza definiendo el estado inicial de la FSM. Luego, define una función para procesar una entrada. Esta función busca la transición de estado correspondiente a la entrada actual y, si la encuentra, cambia de estado y realiza la acción correspondiente.

Finalmente, el código procesa una serie de entradas. En este caso, las entradas son "entrada_1", "entrada_2" y "entrada_3". El código llama a la función `procesar_entrada` para cada una de estas entradas, y la FSM cambia de estado y realiza las acciones correspondientes.