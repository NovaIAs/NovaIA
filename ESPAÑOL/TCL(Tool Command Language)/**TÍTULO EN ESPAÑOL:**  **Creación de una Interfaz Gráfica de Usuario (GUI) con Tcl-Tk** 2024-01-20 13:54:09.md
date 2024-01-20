```tcl
# Procedimiento para crear una ventana con interfaz gráfica de usuario (GUI)
proc crearVentana {titulo ancho alto} {
    # Crear una ventana con el título y tamaño especificados
    toplevel .ventana$titulo -title $titulo -width $ancho -height $alto

    # Crear un marco para contener los widgets
    frame .ventana$titulo.marco -relief groove -bd 2

    # Empaquetar el marco en la ventana
    pack .ventana$titulo.marco -expand yes -fill both

    # Devolver la ventana creada
    return .ventana$titulo
}

# Procedimiento para crear un botón
proc crearBoton {ventana texto comando} {
    # Crear un botón con el texto y comando especificados
    button .ventana$ventana.boton$texto -text $texto -command $comando

    # Empaquetar el botón en el marco
    pack .ventana$ventana.boton$texto -side left -padx 5 -pady 5
}

# Procedimiento para crear una etiqueta
proc crearEtiqueta {ventana texto} {
    # Crear una etiqueta con el texto especificado
    label .ventana$ventana.etiqueta$texto -text $texto

    # Empaquetar la etiqueta en el marco
    pack .ventana$ventana.etiqueta$texto -side left -padx 5 -pady 5
}

# Procedimiento para crear un campo de entrada de texto
proc crearEntrada {ventana texto} {
    # Crear un campo de entrada de texto con el texto especificado
    entry .ventana$ventana.entrada$texto -text $texto

    # Empaquetar el campo en el marco
    pack .ventana$ventana.entrada$texto -side left -padx 5 -pady 5
}

# Procedimiento para crear un cuadro de lista
proc crearLista {ventana titulos datos} {
    # Crear un cuadro de lista con los títulos y datos especificados
    listbox .ventana$ventana.lista$titulos -listvariable datos

    # Empaquetar el cuadro de lista en el marco
    pack .ventana$ventana.lista$titulos -side left -padx 5 -pady 5
}

# Crear una ventana principal
set ventanaPrincipal [crearVentana "Ventana Principal" 400 300]

# Crear un botón en la ventana principal
crearBoton $ventanaPrincipal "Hola Mundo" {puts "Hola Mundo!"}

# Crear una etiqueta en la ventana principal
crearEtiqueta $ventanaPrincipal "Nombre:"

# Crear un campo de entrada de texto en la ventana principal
crearEntrada $ventanaPrincipal ""

# Crear un cuadro de lista en la ventana principal
crearLista $ventanaPrincipal {"Nombre" "Apellido" "Dirección"} {}

# Iniciar el ciclo de eventos de la interfaz gráfica de usuario
tkwait window $ventanaPrincipal
```

**Explicación:**

Este código crea una interfaz gráfica de usuario (GUI) en Tcl/Tk que consta de una ventana principal con un botón, una etiqueta, un campo de entrada de texto y un cuadro de lista.

* La ventana principal se crea utilizando el procedimiento `crearVentana` que toma como parámetros el título, el ancho y el alto de la ventana.
* El botón se crea utilizando el procedimiento `crearBoton` que toma como parámetros la ventana, el texto del botón y el comando que se ejecutará cuando se haga clic en el botón.
* La etiqueta se crea utilizando el procedimiento `crearEtiqueta` que toma como parámetros la ventana y el texto de la etiqueta.
* El campo de entrada de texto se crea utilizando el procedimiento `crearEntrada` que toma como parámetros la ventana y el texto inicial del campo.
* El cuadro de lista se crea utilizando el procedimiento `crearLista` que toma como parámetros la ventana, los títulos de las columnas y los datos a mostrar en el cuadro de lista.
* Finalmente, el ciclo de eventos de la GUI se inicia utilizando el comando `tkwait window $ventanaPrincipal`. Este comando hace que la GUI espere a que el usuario interactúe con ella, como hacer clic en un botón o ingresar texto en un campo de entrada. Cuando el usuario interactúa con la GUI, el código correspondiente al evento se ejecuta.