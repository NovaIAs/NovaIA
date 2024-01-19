```tcl
# Crear una nueva ventana
wm title "Mi Aplicación TCL"
toplevel .main

# Añadir un marco para los controles
frame .controls -relief raised -bd 2
pack .controls -side top -fill x

# Añadir un botón para abrir un archivo
button .controls.open -text "Abrir Archivo" -command {
    set filename [tk_getOpenFile -title "Seleccione un Archivo"]
    if {$filename != ""} {
        # Leer el contenido del archivo
        set filedata [read $filename]

        # Mostrar el contenido del archivo en un cuadro de texto
        text .main.text -state normal
        .main.text insert 1.0 $filedata
        .main.text -state disabled
    }
}
pack .controls.open -side left -padx 10 -pady 10

# Añadir un botón para guardar un archivo
button .controls.save -text "Guardar Archivo" -command {
    set filename [tk_getSaveFile -title "Guardar Archivo"]
    if {$filename != ""} {
        # Obtener el contenido del cuadro de texto
        set filedata [.main.text get 1.0 end]

        # Escribir el contenido en el archivo
        write $filename $filedata
    }
}
pack .controls.save -side left -padx 10 -pady 10

# Añadir un botón para salir de la aplicación
button .controls.quit -text "Salir" -command {exit}
pack .controls.quit -side right -padx 10 -pady 10

# Añadir un cuadro de texto para mostrar el contenido del archivo
text .main.text -height 10 -width 40 -state disabled
pack .main.text -side top -fill both -expand true

# Iniciar el bucle principal de la aplicación
mainloop
```

Explicación del código:

1. Se crea una nueva ventana con el título "Mi Aplicación TCL".
2. Se añade un marco para los controles, que contiene los botones para abrir y guardar un archivo, así como un botón para salir de la aplicación.
3. Se añade un botón para abrir un archivo, que permite al usuario seleccionar un archivo para abrir.
4. Se añade un botón para guardar un archivo, que permite al usuario guardar el contenido del cuadro de texto en un archivo.
5. Se añade un botón para salir de la aplicación, que cierra la aplicación.
6. Se añade un cuadro de texto para mostrar el contenido del archivo.
7. Se inicia el bucle principal de la aplicación, que espera a que el usuario interactúe con los controles.

Este código es un ejemplo simple de una aplicación TCL que permite al usuario abrir y guardar archivos. Es un buen punto de partida para crear aplicaciones más complejas.