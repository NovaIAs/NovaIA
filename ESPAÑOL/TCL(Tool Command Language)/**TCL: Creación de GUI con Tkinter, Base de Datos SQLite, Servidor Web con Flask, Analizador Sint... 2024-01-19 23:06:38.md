```tcl
# Crear una interfaz gráfica de usuario (GUI) con Tkinter

# Importar las bibliotecas necesarias
import tkinter as tk
from tkinter import ttk

# Crear la ventana principal
ventana = tk.Tk()
ventana.title("Ejemplo de GUI en TCL")

# Crear una etiqueta
etiqueta = ttk.Label(ventana, text="¡Hola, mundo!")
etiqueta.pack()

# Crear un botón
boton = ttk.Button(ventana, text="Presionar")
boton.pack()

# Definir la función que se ejecutará cuando se presione el botón
def presionar():
    print("¡Hola, mundo!")

# Asociar la función al botón
boton.configure(command=presionar)

# Iniciar el bucle principal de la GUI
ventana.mainloop()


# Crear una base de datos SQLite

# Importar la biblioteca sqlite3
import sqlite3

# Crear una conexión a la base de datos
conexion = sqlite3.connect("ejemplo.db")

# Crear un cursor
cursor = conexion.cursor()

# Crear una tabla
cursor.execute("CREATE TABLE IF NOT EXISTS ejemplo (id INTEGER PRIMARY KEY, nombre TEXT, edad INTEGER)")

# Insertar datos en la tabla
cursor.execute("INSERT INTO ejemplo (nombre, edad) VALUES ('Juan', 20)")
cursor.execute("INSERT INTO ejemplo (nombre, edad) VALUES ('María', 25)")
cursor.execute("INSERT INTO ejemplo (nombre, edad) VALUES ('Pedro', 30)")

# Confirmar los cambios en la base de datos
conexion.commit()

# Cerrar la conexión a la base de datos
conexion.close()


# Crear un servidor web con Flask

# Importar la biblioteca Flask
from flask import Flask, render_template

# Crear una aplicación Flask
app = Flask(__name__)

# Definir la ruta principal
@app.route("/")
def index():
    return render_template("index.html")

# Iniciar el servidor web
app.run()


# Crear un analizador sintáctico para un lenguaje de programación

# Importar la biblioteca ply
import ply.lex as lex
import ply.yacc as yacc

# Definir los tokens del lenguaje
tokens = (
    'NUMBER',
    'PLUS',
    'MINUS',
    'TIMES',
    'DIVIDE',
    'LPAREN',
    'RPAREN',
)

# Definir las reglas de producción del lenguaje
def p_expression_plus(p):
    'expression : expression PLUS expression'
    p[0] = p[1] + p[3]

def p_expression_minus(p):
    'expression : expression MINUS expression'
    p[0] = p[1] - p[3]

def p_expression_times(p):
    'expression : expression TIMES expression'
    p[0] = p[1] * p[3]

def p_expression_divide(p):
    'expression : expression DIVIDE expression'
    p[0] = p[1] / p[3]

def p_expression_number(p):
    'expression : NUMBER'
    p[0] = p[1]

def p_expression_paren(p):
    'expression : LPAREN expression RPAREN'
    p[0] = p[2]

# Definir el analizador léxico
lexer = lex.lex()

# Definir el analizador sintáctico
parser = yacc.yacc()

# Analizar una expresión
expresion = "1 + 2 * 3"
resultado = parser.parse(expresión)

print(resultado)
```