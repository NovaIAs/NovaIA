```tcl
# Este código crea una ventana principal, un menú principal y una barra de estado.

# Crear la ventana principal
wm title "Ventana principal"
wm geometry "640x480"

# Crear el menú principal
menu .main {
    tearoff false
    postcascade @w 0 0
}

# Agregar elementos al menú principal
menu .main add command -label "Archivo"  -underline 0 -accelerator "Ctrl+F" -command {.f menu open}
menu .main add command -label "Editar"  -underline 0 -accelerator "Ctrl+E" -command {.f menu edit}
menu .main add command -label "Ver"     -underline 0 -accelerator "Ctrl+V" -command {.f menu view}
menu .main add command -label "Ayuda"   -underline 0 -accelerator "Ctrl+H" -command {.f menu help}

# Crear la barra de estado
set status_bar [label .status_bar -text "Barra de estado" -relief sunken -bd 1 -anchor sw]
pack $status_bar -side bottom -fill x

# Mostrar la ventana principal
wm deiconify .

# Definir las funciones del menú principal

proc {.f menu open} {} {
    puts "Se ha abierto un archivo."
}

proc {.f menu edit} {} {
    puts "Se ha editado un archivo."
}

proc {.f menu view} {} {
    puts "Se ha cambiado la vista."
}

proc {.f menu help} {} {
    puts "Se ha mostrado la ayuda."
}
```

Explicación del código:

* La primera línea crea la ventana principal con el título "Ventana principal" y las dimensiones 640x480.
* La segunda línea crea el menú principal.
* La tercera línea agrega elementos al menú principal, incluyendo los elementos "Archivo", "Editar", "Ver" y "Ayuda".
* La cuarta línea crea la barra de estado.
* La quinta línea muestra la ventana principal.
* Las últimas cuatro líneas definen las funciones del menú principal.