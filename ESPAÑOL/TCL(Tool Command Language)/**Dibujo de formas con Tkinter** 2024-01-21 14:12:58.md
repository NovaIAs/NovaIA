```tcl
# Este código crea una interfaz gráfica de usuario (GUI) que permite al usuario dibujar formas en una ventana.

# Importamos la biblioteca Tkinter, que proporciona las clases y funciones necesarias para crear interfaces gráficas de usuario en Python.
import tkinter as tk

# Creamos la ventana principal de la aplicación. Le damos un título y un tamaño.
ventana = tk.Tk()
ventana.title("Dibujo de formas")
ventana.geometry("500x500")

# Creamos un lienzo, que es el área de dibujo. Le damos un tamaño y un color de fondo.
lienzo = tk.Canvas(ventana, width=500, height=500, bg="white")
lienzo.pack()

# Creamos una variable para almacenar la forma que se está dibujando actualmente.
forma = ""

# Creamos una función para dibujar una línea.
def dibujar_linea(evento):
    global forma

    # Si la forma actual es una línea, la dibujamos y la guardamos en la variable forma.
    if forma == "linea":
        lienzo.create_line(evento.x, evento.y, evento.x + 1, evento.y + 1, fill="black")
        forma = "linea"

# Creamos una función para dibujar un círculo.
def dibujar_circulo(evento):
    global forma

    # Si la forma actual es un círculo, lo dibujamos y lo guardamos en la variable forma.
    if forma == "circulo":
        lienzo.create_oval(evento.x - 10, evento.y - 10, evento.x + 10, evento.y + 10, fill="black")
        forma = "circulo"

# Creamos una función para dibujar un cuadrado.
def dibujar_cuadrado(evento):
    global forma

    # Si la forma actual es un cuadrado, lo dibujamos y lo guardamos en la variable forma.
    if forma == "cuadrado":
        lienzo.create_rectangle(evento.x - 10, evento.y - 10, evento.x + 10, evento.y + 10, fill="black")
        forma = "cuadrado"

# Creamos una función para dibujar un triángulo.
def dibujar_triangulo(evento):
    global forma

    # Si la forma actual es un triángulo, lo dibujamos y lo guardamos en la variable forma.
    if forma == "triangulo":
        lienzo.create_polygon(evento.x - 10, evento.y - 10, evento.x + 10, evento.y - 10, evento.x, evento.y + 10, fill="black")
        forma = "triangulo"

# Creamos una función para borrar el lienzo.
def borrar_lienzo():
    lienzo.delete("all")

# Creamos un menú para seleccionar la forma que se quiere dibujar.
menu = tk.Menu(ventana)
ventana.config(menu=menu)

# Creamos un submenú para las formas.
menu_formas = tk.Menu(menu, tearoff=0)
menu.add_cascade(label="Formas", menu=menu_formas)

# Añadimos las opciones del submenú de las formas.
menu_formas.add_command(label="Línea", command=dibujar_linea)
menu_formas.add_command(label="Círculo", command=dibujar_circulo)
menu_formas.add_command(label="Cuadrado", command=dibujar_cuadrado)
menu_formas.add_command(label="Triángulo", command=dibujar_triangulo)

# Creamos un botón para borrar el lienzo.
boton_borrar = tk.Button(ventana, text="Borrar", command=borrar_lienzo)
boton_borrar.pack()

# Iniciamos el bucle de eventos de la aplicación. Esto permite que la aplicación responda a los eventos del usuario, como los clics del ratón y las pulsaciones de teclas.
ventana.mainloop()
```

Este código es un ejemplo de una interfaz gráfica de usuario (GUI) que permite al usuario dibujar formas en una ventana. La GUI está creada con la biblioteca Tkinter, que es una biblioteca de Python para crear interfaces gráficas de usuario.

El código primero importa la biblioteca Tkinter y luego crea la ventana principal de la aplicación. La ventana tiene un título y un tamaño.

A continuación, el código crea un lienzo, que es el área de dibujo. El lienzo tiene un tamaño y un color de fondo.

El código también crea una variable para almacenar la forma que se está dibujando actualmente.

Luego, el código crea una función para dibujar una línea, un círculo, un cuadrado y un triángulo. Cada función dibuja la forma correspondiente en el lienzo y la guarda en la variable forma.

El código también crea una función para borrar el lienzo.

A continuación, el código crea un menú para seleccionar la forma que se quiere dibujar. El menú tiene un submenú para las formas, que contiene las opciones para dibujar una línea, un círculo, un cuadrado y un triángulo.

El código también crea un botón para borrar el lienzo.

Finalmente, el código inicia el bucle de eventos de la aplicación. Esto permite que la aplicación responda a los eventos del usuario, como los clics del ratón y las pulsaciones de teclas.