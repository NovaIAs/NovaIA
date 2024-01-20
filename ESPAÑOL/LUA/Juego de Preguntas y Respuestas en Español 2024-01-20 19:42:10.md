```lua
-- Este código es un juego de preguntas y respuestas en español.

-- Definimos las preguntas y respuestas.
preguntas = {
"Cuál es la capital de España?",
"Cuál es el río más largo del mundo?",
"Cuál es la montaña más alta del mundo?",
"Cuál es el país más poblado del mundo?",
"Cuál es el animal más grande del mundo?",
"Cuál es el planeta más cercano al Sol?",
"Cuál es el planeta más grande del Sistema Solar?",
"Cuál es la estrella más cercana a la Tierra?",
"Cuál es la galaxia más cercana a la Vía Láctea?",
"Cuál es el universo más cercano al nuestro?"
}

respuestas = {
"Madrid",
"Nilo",
"Monte Everest",
"China",
"Ballena azul",
"Mercurio",
"Júpiter",
"Sol",
"Andrómeda",
"Centauro"
}

-- Definimos la función que hace las preguntas.
function hacerPregunta(pregunta)
  print(pregunta)
  respuesta = io.read()
  return respuesta
end

-- Definimos la función que comprueba las respuestas.
function comprobarRespuesta(respuesta, respuesta_correcta)
  if respuesta == respuesta_correcta then
    print("Correcto!")
  else
    print("Incorrecto. La respuesta correcta es", respuesta_correcta)
  end
end

-- Definimos la función que juega el juego.
function jugarJuego()
  for i = 1, #preguntas do
    pregunta = preguntas[i]
    respuesta = hacerPregunta(pregunta)
    respuesta_correcta = respuestas[i]
    comprobarRespuesta(respuesta, respuesta_correcta)
  end
end

-- Jugamos el juego.
jugarJuego()
```

Explicación del código:

* El código primero define las preguntas y respuestas en dos tablas separadas.
* Luego, se define la función `hacerPregunta` que hace una pregunta al usuario y devuelve su respuesta.
* La función `comprobarRespuesta` comprueba si la respuesta del usuario es correcta o no.
* Finalmente, se define la función `jugarJuego` que juega el juego haciendo preguntas al usuario y comprobando sus respuestas.
* El juego se juega llamando a la función `jugarJuego()`.