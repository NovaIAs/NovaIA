```python
# Librerías necesarias
import tkinter as tk
from tkinter import ttk
import sqlite3
import matplotlib.pyplot as plt
import numpy as np

# Crear la ventana principal
ventana = tk.Tk()
ventana.title("Gestión de clientes")

# Crear la base de datos
conexion = sqlite3.connect("clientes.db")
cursor = conexion.cursor()
cursor.execute("CREATE TABLE IF NOT EXISTS clientes (id INTEGER PRIMARY KEY, nombre TEXT, apellido TEXT, email TEXT, telefono TEXT)")
conexion.commit()

# Crear el formulario de entrada de datos
label_nombre = ttk.Label(ventana, text="Nombre:")
label_nombre.grid(row=0, column=0)
entrada_nombre = ttk.Entry(ventana)
entrada_nombre.grid(row=0, column=1)

label_apellido = ttk.Label(ventana, text="Apellido:")
label_apellido.grid(row=1, column=0)
entrada_apellido = ttk.Entry(ventana)
entrada_apellido.grid(row=1, column=1)

label_email = ttk.Label(ventana, text="Email:")
label_email.grid(row=2, column=0)
entrada_email = ttk.Entry(ventana)
entrada_email.grid(row=2, column=1)

label_telefono = ttk.Label(ventana, text="Teléfono:")
label_telefono.grid(row=3, column=0)
entrada_telefono = ttk.Entry(ventana)
entrada_telefono.grid(row=3, column=1)

# Crear los botones de acción
boton_guardar = ttk.Button(ventana, text="Guardar")
boton_guardar.grid(row=4, column=0)
boton_eliminar = ttk.Button(ventana, text="Eliminar")
boton_eliminar.grid(row=4, column=1)
boton_actualizar = ttk.Button(ventana, text="Actualizar")
boton_actualizar.grid(row=4, column=2)
boton_graficar = ttk.Button(ventana, text="Graficar")
boton_graficar.grid(row=4, column=3)

# Crear la tabla de datos
tabla_datos = ttk.Treeview(ventana)
tabla_datos.grid(row=5, columnspan=4)
tabla_datos["columns"] = ("id", "nombre", "apellido", "email", "telefono")
tabla_datos.column("#0", width=0, stretch=False)
tabla_datos.column("id", anchor=tk.CENTER, width=50)
tabla_datos.column("nombre", anchor=tk.CENTER, width=150)
tabla_datos.column("apellido", anchor=tk.CENTER, width=150)
tabla_datos.column("email", anchor=tk.CENTER, width=200)
tabla_datos.column("telefono", anchor=tk.CENTER, width=100)

# Función para guardar los datos
def guardar_datos():
    nombre = entrada_nombre.get()
    apellido = entrada_apellido.get()
    email = entrada_email.get()
    telefono = entrada_telefono.get()
    if nombre and apellido and email and telefono:
        cursor.execute("INSERT INTO clientes (nombre, apellido, email, telefono) VALUES (?, ?, ?, ?)", (nombre, apellido, email, telefono))
        conexion.commit()
        tabla_datos.insert("", tk.END, values=(cursor.lastrowid, nombre, apellido, email, telefono))
        entrada_nombre.delete(0, tk.END)
        entrada_apellido.delete(0, tk.END)
        entrada_email.delete(0, tk.END)
        entrada_telefono.delete(0, tk.END)

# Función para eliminar los datos
def eliminar_datos():
    fila_seleccionada = tabla_datos.selection()
    if fila_seleccionada:
        id = tabla_datos.item(fila_seleccionada)["values"][0]
        cursor.execute("DELETE FROM clientes WHERE id = ?", (id,))
        conexion.commit()
        tabla_datos.delete(fila_seleccionada)

# Función para actualizar los datos
def actualizar_datos():
    fila_seleccionada = tabla_datos.selection()
    if fila_seleccionada:
        id = tabla_datos.item(fila_seleccionada)["values"][0]
        nombre = entrada_nombre.get()
        apellido = entrada_apellido.get()
        email = entrada_email.get()
        telefono = entrada_telefono.get()
        if nombre and apellido and email and telefono:
            cursor.execute("UPDATE clientes SET nombre = ?, apellido = ?, email = ?, telefono = ? WHERE id = ?", (nombre, apellido, email, telefono, id))
            conexion.commit()
            tabla_datos.item(fila_seleccionada, values=(id, nombre, apellido, email, telefono))
            entrada_nombre.delete(0, tk.END)
            entrada_apellido.delete(0, tk.END)
            entrada_email.delete(0, tk.END)
            entrada_telefono.delete(0, tk.END)

# Función para graficar los datos
def graficar_datos():
    cursor.execute("SELECT nombre, apellido, telefono FROM clientes")
    datos = cursor.fetchall()
    nombres = [dato[0] for dato in datos]
    apellidos = [dato[1] for dato in datos]
    telefonos = [dato[2] for dato in datos]
    plt.bar(nombres, telefonos, color="blue", width=0.5)
    plt.xticks(rotation=45)
    plt.xlabel("Nombres y Apellidos")
    plt.ylabel("Teléfonos")
    plt.title("Relación entre Nombres, Apellidos y Teléfonos")
    plt.show()

# Eventos de los botones
boton_guardar.configure(command=guardar_datos)
boton_eliminar.configure(command=eliminar_datos)
boton_actualizar.configure(command=actualizar_datos)
boton_graficar.configure(command=graficar_datos)

# Loop principal
ventana.mainloop()

# Cerrar la conexión a la base de datos
cursor.close()
conexion.close()
```

Este código crea una aplicación de gestión de clientes en Python utilizando la biblioteca tkinter. La aplicación permite añadir, eliminar, actualizar y graficar los datos de los clientes. La base de datos se crea utilizando la biblioteca sqlite3.

El código está dividido en varias funciones, que se explican a continuación:

* `guardar_datos()`: Esta función guarda los datos introducidos en el formulario en la base de datos.
* `eliminar_datos()`: Esta función elimina los datos del cliente seleccionado de la base de datos.
* `actualizar_datos()`: Esta función actualiza los datos del cliente seleccionado en la base de datos.
* `graficar_datos()`: Esta función genera un gráfico de barras que muestra la relación entre los nombres, apellidos y teléfonos de los clientes.

El código principal crea la ventana principal, los widgets y los eventos de los botones. También crea la conexión a la base de datos y la cierra cuando la aplicación se cierra.