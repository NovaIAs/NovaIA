```tcl
# Definimos el procedimiento principal de nuestro programa
proc main {args} {
    # Creamos una ventana de Tkinter para la interfaz gráfica de usuario
    set root [tk_mainwindow .]

    # Creamos un marco contenedor para organizar los elementos de la interfaz
    set frame [tk_frame $root]
    $frame pack

    # Creamos una etiqueta para mostrar un mensaje al usuario
    set label [tk_label $frame -text "Bienvenido a mi programa en TCL"]
    $label pack

    # Creamos un botón que se llamará "Presiona aquí"
    set button [tk_button $frame -text "Presiona aquí"]
    $button pack

    # Definimos el comportamiento del botón cuando se hace clic
    $button configure -command [list presionaAqui]

    # Iniciamos el bucle principal de Tkinter
    tk_mainloop
}

# Definimos el procedimiento presionaAqui que se llamará cuando se haga clic en el botón
proc presionaAqui {} {
    # Mostramos un mensaje en la consola
    puts "Hola, mundo!"

    # Obtenemos el valor del campo de entrada
    set valorCampo [tk_entryget $entrada]

    # Mostramos el valor del campo de entrada en una etiqueta
    set label [tk_label $frame -text "Hola, $valorCampo"]
    $label pack
}

# Iniciamos el programa
main
```

Explicación del código:

1. Definimos el procedimiento `main`, que es el procedimiento principal de nuestro programa.

2. Creamos una ventana de Tkinter para la interfaz gráfica de usuario mediante `tk_mainwindow`.

3. Creamos un marco contenedor para organizar los elementos de la interfaz mediante `tk_frame`.

4. Creamos una etiqueta para mostrar un mensaje al usuario mediante `tk_label`.

5. Creamos un botón que se llamará "Presiona aquí" mediante `tk_button`.

6. Definimos el comportamiento del botón cuando se hace clic mediante `$button configure -command [list presionaAqui]`.

7. Iniciamos el bucle principal de Tkinter mediante `tk_mainloop`.

8. Definimos el procedimiento `presionaAqui`, que se llamará cuando se haga clic en el botón.

9. Mostramos un mensaje en la consola mediante `puts`.

10. Obtenemos el valor del campo de entrada mediante `tk_entryget`.

11. Mostramos el valor del campo de entrada en una etiqueta mediante `tk_label`.

12. Iniciamos el programa llamando a `main`.