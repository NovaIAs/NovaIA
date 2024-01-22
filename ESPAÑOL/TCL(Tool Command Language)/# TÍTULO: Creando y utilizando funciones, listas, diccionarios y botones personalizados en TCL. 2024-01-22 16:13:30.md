```tcl
# Definición de una nueva función en TCL llamada "mi_función".

proc mi_función {argumento_1 argumento_2} {
    # Esta función toma dos argumentos y los imprime en la consola.

    puts "Argumento 1: $argumento_1"
    puts "Argumento 2: $argumento_2"
}

# Definición de una nueva lista en TCL llamada "mi_lista".

set mi_lista {uno dos tres}

# Iteración sobre la lista "mi_lista" utilizando un bucle "foreach".

foreach elemento $mi_lista {
    # Este bucle imprime cada elemento de la lista en la consola.

    puts "Elemento: $elemento"
}

# Definición de un nuevo diccionario en TCL llamado "mi_diccionario".

dict set mi_diccionario {
    nombre "Juan Pérez"
    edad 25
    ciudad "Madrid"
}

# Obtención del valor de una clave específica en el diccionario "mi_diccionario".

set nombre [dict get mi_diccionario nombre]

# Impresión del valor de la clave "nombre" en la consola.

puts "Nombre: $nombre"

# Creación de un objeto de clase "Button" en TCL utilizando la función "button".

button .mi_boton -text "Mi botón" -command {puts "Botón pulsado!"}

# Empaquetado del botón en la ventana principal mediante el comando "pack".

pack .mi_boton

# Ejecución del bucle principal de la aplicación TCL.

mainloop
```

# Explicación:

* Este código define la nueva función `mi_función` que toma dos argumentos y los imprime en la consola.
* También se define una lista `mi_lista` que contiene tres elementos.
* Luego un bucle `foreach` itera sobre la lista imprimiendo cada elemento en la consola.
* Se crea un diccionario `mi_diccionario` que contiene tres claves: nombre, edad y ciudad.
* Se obtiene el valor de la clave nombre del diccionario y se guarda en la variable `nombre`.
* Se imprime el valor de la clave nombre en la consola.
* Se crea un botón usando el comando `button` y se le añade el texto "Mi botón" y un comando que se ejecutará cuando se pulse el botón.
* Se empaqueta el botón en la ventana principal usando el comando `pack`.
* Se ejecuta el bucle principal de la aplicación usando el comando `mainloop`.