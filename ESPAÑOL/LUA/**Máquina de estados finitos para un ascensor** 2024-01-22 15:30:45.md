```lua
-- Este código crea una máquina de estados finitos simple para simular el comportamiento de un ascensor.

-- Definimos los estados posibles del ascensor.
estados = {
  "parado",
  "subiendo",
  "bajando",
  "puerta abierta"
}

-- Definimos las transiciones posibles entre estados.
transiciones = {
  { "parado", "subiendo", "subir" },
  { "parado", "bajando", "bajar" },
  { "parado", "puerta abierta", "abrir puerta" },
  { "subiendo", "parado", "parar" },
  { "bajando", "parado", "parar" },
  { "puerta abierta", "parado", "cerrar puerta" }
}

-- Definimos el estado inicial del ascensor.
estado_inicial = "parado"

-- Creamos una función para procesar un evento.
procesar_evento = function(evento)
  -- Buscamos la transición correspondiente al evento.
  for i = 1, #transiciones do
    if transiciones[i][1] == estado_actual and transiciones[i][3] == evento then
      -- Si encontramos la transición, cambiamos el estado del ascensor.
      estado_actual = transiciones[i][2]

      -- Ejecutamos la acción asociada a la transición.
      print("Acción:", transiciones[i][4])

      break
    end
  end
end

-- Inicializamos el estado actual del ascensor.
estado_actual = estado_inicial

-- Procesamos algunos eventos.
procesar_evento("subir")
procesar_evento("abrir puerta")
procesar_evento("cerrar puerta")
procesar_evento("bajar")
procesar_evento("parar")

```

Explicación del código:

* El código define una máquina de estados finitos simple para simular el comportamiento de un ascensor.
* La máquina de estados finitos está definida por un conjunto de estados, un conjunto de transiciones entre estados y un estado inicial.
* El código define los estados posibles del ascensor, las transiciones posibles entre estados y el estado inicial del ascensor.
* El código también define una función para procesar eventos.
* La función para procesar eventos toma un evento como argumento y busca la transición correspondiente al evento.
* Si encuentra la transición, cambia el estado del ascensor y ejecuta la acción asociada a la transición.
* El código inicializa el estado actual del ascensor y luego procesa algunos eventos.