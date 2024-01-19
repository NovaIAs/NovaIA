```tcl
# Declaración de bibliotecas
package require Tcl 8.5
package require Tk 8.5

# Crear la ventana principal
set main [tk toplevel]

# Configurar la ventana principal
tk title $main "Ventana Principal"
tk geometry $main 300x200

# Crear un marco para contener los widgets
set frame [tk frame $main]

# Configurar el marco
tk pack $frame -side top -expand yes -fill both

# Crear una etiqueta de texto
set label [tk label $frame -text "Hola, mundo!"]

# Configurar la etiqueta de texto
tk pack $label -side top -pady 10

# Crear un botón
set button [tk button $frame -text "Presiona aquí"]

# Configurar el botón
tk pack $button -side top -pady 10

# Definir el comando a ejecutar cuando se presione el botón
tk command $button -command {
    # Obtener el texto actual de la etiqueta
    set text [tk cget $label -text]

    # Cambiar el texto de la etiqueta
    tk configure $label -text "Hola, $text!"
}

# Iniciar el bucle principal de la aplicación
tk mainloop
```

**Explicación del código:**

1. **Declaración de bibliotecas:** Este código utiliza las bibliotecas `Tcl` y `Tk`, que son necesarias para crear interfaces gráficas de usuario en Tcl. Estas bibliotecas se cargan usando los comandos `package require`.
2. **Crear la ventana principal:** La ventana principal es creada usando el comando `tk toplevel`. Ésta es la ventana que contiene todos los demás widgets.
3. **Configurar la ventana principal:** La ventana principal es configurada con el comando `tk title` para establecer su título y el comando `tk geometry` para establecer su tamaño y posición.
4. **Crear un marco para contener los widgets:** El marco es creado usando el comando `tk frame`. Éste es un contenedor que se utiliza para organizar los widgets dentro de la ventana principal.
5. **Configurar el marco:** El marco es configurado con el comando `tk pack` para que ocupe todo el espacio disponible en la ventana principal.
6. **Crear una etiqueta de texto:** La etiqueta de texto es creada usando el comando `tk label`. Ésta es una ventana que muestra texto.
7. **Configurar la etiqueta de texto:** La etiqueta de texto es configurada con el comando `tk pack` para que ocupe un espacio específico dentro del marco y el comando `tk configure` para establecer su texto.
8. **Crear un botón:** El botón es creado usando el comando `tk button`. Éste es un widget que permite al usuario interactuar con la interfaz gráfica de usuario.
9. **Configurar el botón:** El botón es configurado con el comando `tk pack` para que ocupe un espacio específico dentro del marco y el comando `tk command` para especificar qué comando se debe ejecutar cuando se presione el botón.
10. **Definir el comando a ejecutar cuando se presione el botón:** El comando a ejecutar cuando se presione el botón es definido usando el comando `tk command`. Éste es un comando que obtiene el texto actual de la etiqueta y lo reemplaza con un nuevo texto.
11. **Iniciar el bucle principal de la aplicación:** El bucle principal de la aplicación es iniciado usando el comando `tk mainloop`. Éste es un bucle que se ejecuta hasta que el usuario cierre la ventana principal.