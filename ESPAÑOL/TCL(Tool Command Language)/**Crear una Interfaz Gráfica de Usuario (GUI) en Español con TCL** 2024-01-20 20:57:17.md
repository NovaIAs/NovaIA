```tcl
# Crear una interfaz gráfica de usuario (GUI) en español

# Importar las bibliotecas necesarias
package require Tk

# Crear la ventana principal
set window .main
toplevel $window

# Establecer el título de la ventana
wm title $window "Mi programa en español"

# Crear un marco para los botones
frame .frame
pack $frame -side top -padx 10 -pady 10

# Crear un botón para saludar
button .saludar -text "Saludar"
pack $saludar -in $frame -side left -padx 5 -pady 5

# Crear un botón para despedirse
button .despedirse -text "Despedirse"
pack $despedirse -in $frame -side left -padx 5 -pady 5

# Crear un botón para salir
button .salir -text "Salir"
pack $salir -in $frame -side left -padx 5 -pady 5

# Definir el comportamiento del botón "Saludar"
bind $saludar <Button-1> {
    # Mostrar un mensaje de saludo
    set mensaje "Hola, mundo!"
    messagebox -message $mensaje -title "Saludo" -type ok
}

# Definir el comportamiento del botón "Despedirse"
bind $despedirse <Button-1> {
    # Mostrar un mensaje de despedida
    set mensaje "Adiós, mundo!"
    messagebox -message $mensaje -title "Despedida" -type ok
}

# Definir el comportamiento del botón "Salir"
bind $salir <Button-1> {
    # Salir del programa
    exit
}

# Mostrar la ventana principal
wm deiconify $window

# Ejecutar el programa
mainloop
```

Explicación del código:

* El código comienza importando la biblioteca `Tk`, que es necesaria para crear interfaces gráficas de usuario (GUI) en Tcl.
* A continuación, se crea la ventana principal, que es la ventana principal del programa.
* Se establece el título de la ventana, que es "Mi programa en español".
* Se crea un marco para los botones.
* Se crean tres botones: "Saludar", "Despedirse" y "Salir".
* Se definen los comportamientos de los botones.
* El botón "Saludar" muestra un mensaje de saludo cuando se hace clic en él.
* El botón "Despedirse" muestra un mensaje de despedida cuando se hace clic en él.
* El botón "Salir" sale del programa cuando se hace clic en él.
* Por último, se muestra la ventana principal y se ejecuta el programa.