```tcl
# Este código crea una interfaz gráfica de usuario (GUI) simple que permite al usuario ingresar texto
# y luego muestra el texto ingresado en una ventana de texto.

# Importar las bibliotecas Tk y ttk.
package require Tk
package require ttk

# Crear la ventana principal.
set mainWindow [ttk::window . -title "Ejemplo de GUI en TCL"]

# Crear un cuadro de texto para que el usuario ingrese texto.
set textEntry [ttk::entry $mainWindow -width 20]
pack $textEntry

# Crear un botón para que el usuario haga clic para mostrar el texto ingresado.
set showTextButton [ttk::button $mainWindow -text "Mostrar texto" -command {showText $textEntry}]
pack $showTextButton

# Crear una ventana de texto para mostrar el texto ingresado.
set textWindow [ttk::text $mainWindow -height 10 -width 20]
pack $textWindow

# Definir una función para mostrar el texto ingresado en la ventana de texto.
proc showText {textEntry} {
    # Obtener el texto ingresado del cuadro de texto.
    set text [ttk::entry $textEntry -value]

    # Insertar el texto en la ventana de texto.
    ttk::text $textWindow -insert end "$text\n"
}

# Iniciar el ciclo principal de la GUI.
ttk::mainloop $mainWindow
```

Explicación del código:

* La primera línea importa la biblioteca Tk, que es la biblioteca estándar de TCL para crear interfaces gráficas de usuario.
* La segunda línea importa la biblioteca ttk, que es una biblioteca de extensiones de Tk que proporciona widgets más modernos y elegantes.
* La tercera línea crea la ventana principal de la GUI.
* La cuarta línea crea un cuadro de texto para que el usuario ingrese texto.
* La quinta línea crea un botón para que el usuario haga clic para mostrar el texto ingresado.
* La sexta línea crea una ventana de texto para mostrar el texto ingresado.
* La séptima línea define una función para mostrar el texto ingresado en la ventana de texto.
* La octava línea inicia el ciclo principal de la GUI.