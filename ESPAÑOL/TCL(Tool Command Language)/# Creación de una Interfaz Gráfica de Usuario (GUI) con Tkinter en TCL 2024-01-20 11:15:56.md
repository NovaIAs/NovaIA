```tcl
# Creación de una interfaz gráfica de usuario (GUI) con Tkinter
package require Tk

# Crear la ventana principal
set ventana [new Toplevel]
wm title $ventana "Mi aplicación TCL"

# Crear un marco para el contenido
set marco [new Frame $ventana]
pack $marco -side top -expand yes -fill both

# Crear una etiqueta de texto
set etiqueta [new Label $marco -text "Hola, mundo!"]
pack $etiqueta -side top

# Crear un botón
set boton [new Button $marco -text "Clic aquí"]
pack $boton -side top

# Definir la acción del botón
bind $boton <Button-1> {
    # Mostrar un mensaje en la consola
    puts "El botón ha sido presionado"

    # Cambiar el texto de la etiqueta
    set etiqueta [new Label $marco -text "Presionado!"]
    pack $etiqueta -side top
}

# Iniciar el ciclo principal de la aplicación
mainloop
```

Este código crea una sencilla interfaz gráfica de usuario (GUI) utilizando la biblioteca Tkinter. La GUI consta de una ventana principal, un marco para el contenido, una etiqueta de texto y un botón. Cuando se presiona el botón, se muestra un mensaje en la consola y el texto de la etiqueta cambia a "Presionado!".

Aquí hay una explicación más detallada del código:

* El primer paso es cargar la biblioteca Tkinter utilizando el comando `package require Tk`.
* A continuación, se crea la ventana principal utilizando el comando `new Toplevel`.
* Se establece el título de la ventana utilizando el comando `wm title`.
* Se crea un marco para el contenido utilizando el comando `new Frame`.
* Se empaqueta el marco en la ventana principal utilizando el comando `pack`.
* Se crea una etiqueta de texto utilizando el comando `new Label`.
* Se empaqueta la etiqueta en el marco utilizando el comando `pack`.
* Se crea un botón utilizando el comando `new Button`.
* Se empaqueta el botón en el marco utilizando el comando `pack`.
* Se define la acción del botón utilizando el comando `bind`.
* El comando `puts` se utiliza para mostrar un mensaje en la consola.
* El comando `set etiqueta` se utiliza para cambiar el texto de la etiqueta.
* El comando `mainloop` se utiliza para iniciar el ciclo principal de la aplicación.