```lua
-- Este código crea una interfaz de usuario gráfica (GUI) con un botón y un cuadro de texto.
-- Cuando el usuario hace clic en el botón, se muestra un mensaje en el cuadro de texto.

-- Crear una nueva ventana
ventana = newWindow("Mi ventana")

-- Crear un nuevo botón
boton = newButton("¡Haz clic aquí!")

-- Añadir el botón a la ventana
ventana:addChild(boton)

-- Crear un nuevo cuadro de texto
texto = newTextBox("")

-- Añadir el cuadro de texto a la ventana
ventana:addChild(texto)

-- Crear un nuevo evento para cuando se haga clic en el botón
boton:addEventListener("click", function()
    -- Mostrar un mensaje en el cuadro de texto
    texto:setText("¡Hola, mundo!")
end)

-- Mostrar la ventana
ventana:show()
```

Explicación del código:

* La primera línea crea una nueva ventana llamada "Mi ventana".
* La segunda línea crea un nuevo botón llamado "¡Haz clic aquí!".
* La tercera línea añade el botón a la ventana.
* La cuarta línea crea un nuevo cuadro de texto vacío.
* La quinta línea añade el cuadro de texto a la ventana.
* La sexta línea crea un nuevo evento para cuando se haga clic en el botón.
* La séptima línea muestra un mensaje en el cuadro de texto cuando se hace clic en el botón.
* La octava línea muestra la ventana.