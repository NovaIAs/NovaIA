```tcl
# Importar la librería Tcl/Tk.
package require Tk

# Crear una ventana principal.
set top [toplevel]
wm title $top "Ventana principal"

# Crear un marco de control para colocar los botones.
set controlFrame [frame $top]
pack $controlFrame -side top -fill x

# Crear un botón para abrir una nueva ventana.
set openButton [button $controlFrame -text "Abrir nueva ventana"]
pack $openButton -side left -padx 5

# Crear un botón para cerrar la ventana principal.
set closeButton [button $controlFrame -text "Cerrar ventana principal"]
pack $closeButton -side left -padx 5

# Crear una nueva ventana cuando se haga clic en el botón "Abrir nueva ventana".
bind $openButton <Button-1> [proc {} {
    # Crear una nueva ventana.
    set newWindow [toplevel]
    wm title $newWindow "Nueva ventana"

    # Crear un botón para cerrar la nueva ventana.
    set closeButton [button $newWindow -text "Cerrar nueva ventana"]
    pack $closeButton -side left -padx 5

    # Mostrar la nueva ventana.
    wm deiconify $newWindow
}]

# Cerrar la ventana principal cuando se haga clic en el botón "Cerrar ventana principal".
bind $closeButton <Button-1> [proc {} {
    # Destruir la ventana principal.
    destroy $top
}]

# Mostrar la ventana principal.
wm deiconify $top

# Iniciar el bucle principal de la aplicación.
mainloop
```

Explicación del código:

1. Se importa la librería Tcl/Tk, que proporciona las funciones y objetos necesarios para crear interfaces gráficas de usuario.

2. Se crea una ventana principal utilizando el comando `toplevel`. La ventana se define usando el gestor de ventanas actual (por ejemplo, Windows, macOS o Linux).

3. Se crea un marco de control utilizando el comando `frame`. Este marco se utiliza para colocar los botones dentro de la ventana principal.

4. Se crean dos botones dentro del marco de control utilizando el comando `button`. El primer botón tiene el texto "Abrir nueva ventana" y el segundo tiene el texto "Cerrar ventana principal".

5. Se crea un enlace en el botón "Abrir nueva ventana" que llama a un procedimiento cuando se hace clic en el botón. El procedimiento abre una nueva ventana utilizando el comando `toplevel`. También se crea un botón para cerrar la nueva ventana.

6. Se crea un enlace en el botón "Cerrar ventana principal" que llama a un procedimiento cuando se hace clic en el botón. El procedimiento destruye la ventana principal utilizando el comando `destroy`.

7. Se muestra la ventana principal utilizando el comando `wm deiconify`. Esto hace que la ventana sea visible en la pantalla.

8. Se inicia el bucle principal de la aplicación utilizando el comando `mainloop`. Este comando inicia un bucle de eventos que escucha los eventos del usuario, como pulsar botones o mover el ratón, y los procesa en consecuencia.