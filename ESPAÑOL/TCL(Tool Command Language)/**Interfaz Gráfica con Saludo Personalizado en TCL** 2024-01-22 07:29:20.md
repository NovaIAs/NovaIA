```tcl
# Crea una ventana principal con título y dimensiones personalizadas
wm title "Ventana Principal"
geometry "800x600+200+100"

# Crea un marco para organizar los elementos de la interfaz
frame .marco -relief groove -bd 2

# Crea una etiqueta para mostrar texto en la ventana
label .marco.etiqueta -text "Bienvenido a la Aplicación TCL"

# Crea un botón para cerrar la ventana
button .marco.boton -text "Cerrar" -command {wm destroy .}

# Crea un cuadro de texto para que el usuario pueda ingresar su nombre
entry .marco.nombre -textvariable nombre

# Crea un botón para saludar al usuario
button .marco.saludar -text "Saludar" -command {
    # Recupera el nombre ingresado por el usuario
    set nombre [get .marco.nombre]

    # Muestra un mensaje de saludo en la ventana
    messagebox -message "Hola, ${nombre}!" -title "Saludo"
}

# Empaqueta el marco y sus elementos para que se ajusten a la ventana
pack .marco

# Inicia el ciclo principal de la interfaz gráfica
mainloop
```

Explicación del código:

1. **Crear ventana principal**:
   - `wm title "Ventana Principal"`: define el título de la ventana.
   - `geometry "800x600+200+100"`: establece las dimensiones y la posición de la ventana.

2. **Crear marco**:
   - `frame .marco ...`: crea un marco con borde y relieve personalizados.

3. **Crear elementos de la interfaz**:
   - `label ... -text "Bienvenido a la Aplicación TCL"`: crea una etiqueta con un texto específico.
   - `button ... -text "Cerrar"`: crea un botón con un texto y asigna un comando para cerrar la ventana.
   - `entry ... -textvariable nombre`: crea un cuadro de texto y asigna una variable para almacenar el texto ingresado por el usuario.
   - `button ... -text "Saludar"`: crea un botón con un texto y asigna un comando para saludar al usuario.

4. **Empaquetar elementos**:
   - `pack .marco`: empaqueta el marco y sus elementos dentro de la ventana.

5. **Iniciar ciclo principal**:
   - `mainloop`: inicia el ciclo principal de la interfaz gráfica, permitiendo la interacción del usuario con la ventana.