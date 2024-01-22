```lua
-- Variables para el estado de la aplicación
estado_inicial = "principal"
estado_actual = estado_inicial

-- Función para cargar los recursos de la aplicación
function cargar_recursos()
  png_menu_principal = love.graphics.newImage("menu_principal.png")
  png_juego = love.graphics.newImage("juego.png")
  sonido_musica = love.audio.newSource("musica.ogg")
end

-- Función para actualizar el estado de la aplicación
function actualizar()
  -- Cambiar al estado de juego
  if estado_actual == "principal" and love.keyboard.isDown("enter") then
    estado_actual = "juego"
  end

  -- Cambiar al estado de menú principal
  if estado_actual == "juego" and love.keyboard.isDown("escape") then
    estado_actual = "principal"
  end

  -- Actualizar el estado actual
  if estado_actual == "principal" then
    actualizar_menu_principal()
  elseif estado_actual == "juego" then
    actualizar_juego()
  end
end

-- Función para dibujar el estado de la aplicación
function dibujar()
  -- Dibujar el estado actual
  if estado_actual == "principal" then
    dibujar_menu_principal()
  elseif estado_actual == "juego" then
    dibujar_juego()
  end
end

-- Función para actualizar el estado del menú principal
function actualizar_menu_principal()
  -- Reproducir la música
  sonido_musica:play()
end

-- Función para dibujar el estado del menú principal
function dibujar_menu_principal()
  -- Dibujar la imagen del menú principal
  love.graphics.draw(png_menu_principal, 0, 0)
end

-- Función para actualizar el estado del juego
function actualizar_juego()
  -- Actualizar la posición del jugador
  if love.keyboard.isDown("left") then
    jugador.x = jugador.x - 1
  end
  if love.keyboard.isDown("right") then
    jugador.x = jugador.x + 1
  end
  if love.keyboard.isDown("up") then
    jugador.y = jugador.y - 1
  end
  if love.keyboard.isDown("down") then
    jugador.y = jugador.y + 1
  end
end

-- Función para dibujar el estado del juego
function dibujar_juego()
  -- Dibujar la imagen del juego
  love.graphics.draw(png_juego, 0, 0)

  -- Dibujar al jugador
  love.graphics.circle("fill", jugador.x, jugador.y, 10)
end

-- Función para procesar los eventos que se produzcan
function procesar_eventos()
  -- Manejar los eventos de teclado
  love.keyboard.update()

  -- Manejar los eventos de ratón
  love.mouse.update()
end

-- Función para finalizar la aplicación
function finalizar()
  -- Detener la música
  sonido_musica:stop()
end

-- Función principal
function love.load()
  -- Cargar los recursos
  cargar_recursos()

  -- Definir el jugador
  jugador = { x = 100, y = 100 }
end

-- Función para actualizar la aplicación
function love.update(dt)
  -- Procesar los eventos
  procesar_eventos()

  -- Actualizar el estado de la aplicación
  actualizar()
end

-- Función para dibujar la aplicación
function love.draw()
  -- Dibujar el estado de la aplicación
  dibujar()
end

-- Función para finalizar la aplicación
function love.quit()
  -- Finalizar la aplicación
  finalizar()
end
```

Explicación del código:

* El código comienza con la definición de variables para el estado de la aplicación. Las variables `estado_inicial` y `estado_actual` se utilizan para mantener el estado actual de la aplicación.
* La función `cargar_recursos()` se utiliza para cargar los recursos de la aplicación, como imágenes y sonidos.
* La función `actualizar()` se utiliza para actualizar el estado de la aplicación. En esta función, se comprueba si el estado actual de la aplicación es "principal" o "juego". Si el estado actual es "principal", se llama a la función `actualizar_menu_principal()`. Si el estado actual es "juego", se llama a la función `actualizar_juego()`.
* La función `dibujar()` se utiliza para dibujar el estado de la aplicación. En esta función, se llama a la función `dibujar_menu_principal()` o `dibujar_juego()` en función del estado actual de la aplicación.
* La función `actualizar_menu_principal()` se utiliza para actualizar el estado del menú principal. En esta función, se reproduce la música de fondo.
* La función `dibujar_menu_principal()` se utiliza para dibujar el estado del menú principal. En esta función, se dibuja la imagen del menú principal.
* La función `actualizar_juego()` se utiliza para actualizar el estado del juego. En esta función, se actualiza la posición del jugador en función de las teclas que estén pulsadas.
* La función `dibujar_juego()` se utiliza para dibujar el estado del juego. En esta función, se dibuja la imagen del juego y el jugador.
* La función `procesar_eventos()` se utiliza para procesar los eventos que se produzcan. En esta función, se manejan los eventos de teclado y ratón.
* La función `finalizar()` se utiliza para finalizar la aplicación. En esta función, se detiene la música de fondo.
* La función `love.load()` se llama cuando la aplicación se carga. En esta función, se llaman a las funciones `cargar_recursos()` y `definir_jugador()`.
* La función `love.update()` se llama cada vez que la aplicación se actualiza. En esta función, se llama a la función `procesar_eventos()` y se actualiza el estado de la aplicación llamando a la función `actualizar()`.
* La función `love.draw()` se llama cada vez que la aplicación se dibuja. En esta función, se llama a la función `dibujar()`.
* La función `love.quit()` se llama cuando la aplicación se cierra. En esta función, se llama a la función `finalizar()`.