```tcl
# Crea una ventana principal
wm title "Ventana Principal"
wm geometry "600x400"

# Crea un marco para la aplicación
frame .app -relief raised -borderwidth 1

# Coloca el marco en el centro de la ventana principal
pack .app -expand 1 -fill both

# Crea una etiqueta para el nombre de usuario
label .app.nombreUsuario -text "Nombre de usuario:"

# Crea una entrada de texto para el nombre de usuario
entry .app.entradaNombreUsuario -width 20

# Crea una etiqueta para la contraseña
label .app.contraseña -text "Contraseña:"

# Crea una entrada de texto para la contraseña
entry .app.entradaContraseña -width 20 -show *

# Crea un botón para iniciar sesión
button .app.iniciarSesion -text "Iniciar sesión"

# Empaqueta los widgets en el marco
pack .app.nombreUsuario .app.entradaNombreUsuario \
    .app.contraseña .app.entradaContraseña \
    .app.iniciarSesion

# Define la acción para el botón de inicio de sesión
bind .app.iniciarSesion <Button-1> {
    # Obtener el nombre de usuario y la contraseña de los campos de entrada
    set nombreUsuario [get .app.entradaNombreUsuario]
    set contraseña [get .app.entradaContraseña]

    # Validar el nombre de usuario y la contraseña (en este ejemplo, se asume que son correctos)
    if {[string equal $nombreUsuario "admin"] && [string equal $contraseña "password"]} {
        # Crear una ventana secundaria para mostrar el mensaje de éxito
        wm title .exito "Inicio de sesión exitoso"
        wm geometry .exito "300x200"

        # Crear una etiqueta para el mensaje de éxito
        label .exito.mensaje -text "Inicio de sesión exitoso"

        # Empaquetar la etiqueta en la ventana secundaria
        pack .exito.mensaje

        # Mostrar la ventana secundaria
        toplevel .exito
    } else {
        # Crear una ventana secundaria para mostrar el mensaje de error
        wm title .error "Inicio de sesión fallido"
        wm geometry .error "300x200"

        # Crear una etiqueta para el mensaje de error
        label .error.mensaje -text "Inicio de sesión fallido"

        # Empaquetar la etiqueta en la ventana secundaria
        pack .error.mensaje

        # Mostrar la ventana secundaria
        toplevel .error
    }
}

```

Explicación del código:

1. Se crea una ventana principal con título "Ventana Principal" y se especifica su geometría (tamaño y posición).
2. Se crea un marco para la aplicación y se coloca en el centro de la ventana principal.
3. Se crean varias etiquetas y entradas de texto para el nombre de usuario y la contraseña.
4. Se crea un botón para iniciar sesión.
5. Se definen las acciones para el botón de inicio de sesión, incluyendo la validación del nombre de usuario y la contraseña y la creación de ventanas secundarias para mostrar los mensajes de éxito o error.
6. Se empaquetan los widgets en el marco de la aplicación y se muestra la ventana principal.