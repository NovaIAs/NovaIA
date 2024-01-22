```lua
-- Definiendo funciones y variables
tabla_preguntas = {
  {
    pregunta = "¿Cuál es la capital de España?",
    respuestas = {"Madrid", "Barcelona", "Sevilla", "Valencia"},
    respuesta_correcta = "Madrid",
  },
  {
    pregunta = "¿Cuál es el río más largo del mundo?",
    respuestas = {"Amazonas", "Nilo", "Yangtsé", "Misisipi"},
    respuesta_correcta = "Nilo",
  },
  {
    pregunta = "¿Cuál es la montaña más alta del mundo?",
    respuestas = {"Monte Everest", "K2", "Kangchenjunga", "Lhotse"},
    respuesta_correcta = "Monte Everest",
  },
}

-- Función para imprimir las preguntas y respuestas en la consola
imprimir_pregunta = function(pregunta)
  print(pregunta)
  for _, respuesta in ipairs(pregunta.respuestas) do
    print(respuesta)
  end
end

-- Función para obtener la respuesta del usuario
obtener_respuesta = function()
  print("Ingresa tu respuesta (o 'salir' para terminar): ")
  respuesta = io.read()
  return respuesta
end

-- Función para comprobar si la respuesta es correcta
comprobar_respuesta = function(respuesta, respuesta_correcta)
  if respuesta == respuesta_correcta then
    return true
  else
    return false
  end
end

-- Función principal del juego
main = function()
  -- Recorrer todas las preguntas
  for _, pregunta in ipairs(tabla_preguntas) do
    imprimir_pregunta(pregunta)
    respuesta = obtener_respuesta()

    -- Si el usuario ingresa 'salir', terminar el juego
    if respuesta == "salir" then
      print("Gracias por jugar!")
      return
    end

    -- Comprobar si la respuesta es correcta
    if comprobar_respuesta(respuesta, pregunta.respuesta_correcta) then
      print("¡Correcto!")
    else
      print("¡Incorrecto!")
    end
  end

  -- Imprimir el mensaje final
  print("¡Felicidades, has completado el juego!")
end

-- Iniciar el juego
main()
```

Este código es un juego de preguntas y respuestas en español. El código primero define una tabla de preguntas, cada pregunta tiene una pregunta, una lista de respuestas y una respuesta correcta.

Luego, define tres funciones:

* `imprimir_pregunta` imprime la pregunta y las respuestas en la consola.
* `obtener_respuesta` obtiene la respuesta del usuario.
* `comprobar_respuesta` comprueba si la respuesta del usuario es correcta.

La función `main` es la función principal del juego. Recorre todas las preguntas, imprime la pregunta y las respuestas, obtiene la respuesta del usuario, comprueba si la respuesta es correcta y muestra un mensaje al usuario.

El juego finaliza cuando el usuario ingresa 'salir' o cuando ha respondido todas las preguntas.