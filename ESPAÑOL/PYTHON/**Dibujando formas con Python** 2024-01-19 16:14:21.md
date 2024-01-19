```python
# Importa la biblioteca de gráficos Turtle
import turtle

# Crea una nueva ventana de Turtle
ventana = turtle.Screen()

# Crea una nueva tortuga
tortuga = turtle.Turtle()

# Establece el color de la tortuga
tortuga.color("verde")

# Establece la velocidad de la tortuga
tortuga.speed(10)

# Dibuja un cuadrado
tortuga.forward(100)
tortuga.right(90)
tortuga.forward(100)
tortuga.right(90)
tortuga.forward(100)
tortuga.right(90)
tortuga.forward(100)

# Dibuja un círculo
tortuga.penup()
tortuga.forward(100)
tortuga.pendown()
tortuga.circle(50)

# Dibuja una estrella
tortuga.penup()
tortuga.forward(100)
tortuga.pendown()
for i in range(5):
    tortuga.forward(100)
    tortuga.right(144)

# Espera a que el usuario cierre la ventana
ventana.mainloop()
```

Explicación del código:

* La primera línea importa la biblioteca de gráficos Turtle.
* La segunda línea crea una nueva ventana de Turtle.
* La tercera línea crea una nueva tortuga.
* La cuarta línea establece el color de la tortuga en verde.
* La quinta línea establece la velocidad de la tortuga en 10.
* Las siguientes cuatro líneas dibujan un cuadrado.
* Las siguientes cuatro líneas dibujan un círculo.
* Las siguientes seis líneas dibujan una estrella.
* La última línea espera a que el usuario cierre la ventana.