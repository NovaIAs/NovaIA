```tcl
# Información de encabezado
puts "-----------------------------------------------------------------"
puts "Programa en TCL para gestionar una lista de compras"
puts "-----------------------------------------------------------------"

# Variables para almacenar la lista de compras y el nombre del archivo de texto
set lista_compras {}
set archivo_texto "lista_compras.txt"

# Función para agregar un artículo a la lista de compras
proc agregar_articulo {articulo} {
    # Agrega el artículo a la lista de compras
    lappend lista_compras $articulo

    # Imprime el artículo agregado en la consola
    puts "Artículo agregado:", $articulo
}

# Función para eliminar un artículo de la lista de compras
proc eliminar_articulo {articulo} {
    # Elimina el artículo de la lista de compras
    lreverse lista_compras
    lset lista_compras [lsearch -exact -not $lista_compras $articulo]
    lreverse lista_compras

    # Imprime el artículo eliminado en la consola
    puts "Artículo eliminado:", $articulo
}

# Bucle principal del programa
while {1} {
    # Muestra las opciones al usuario
    puts "\n¿Qué desea hacer?"
    puts "1. Agregar un artículo"
    puts "2. Eliminar un artículo"
    puts "3. Imprimir la lista de compras"
    puts "4. Guardar la lista de compras en un archivo de texto"
    puts "5. Cargar la lista de compras desde un archivo de texto"
    puts "6. Salir"

    # Obtiene la opción del usuario
    set opcion [gets]

    # Ejecuta la acción correspondiente a la opción seleccionada
    switch -exact -- $opcion {
        1 {
            # Solicita al usuario el artículo a agregar
            puts "\nIngrese el artículo a agregar:"
            set articulo [gets]

            # Agrega el artículo a la lista de compras
            agregar_articulo $articulo
        }
        2 {
            # Solicita al usuario el artículo a eliminar
            puts "\nIngrese el artículo a eliminar:"
            set articulo [gets]

            # Elimina el artículo de la lista de compras
            eliminar_articulo $articulo
        }
        3 {
            # Imprime la lista de compras en la consola
            puts "\nLista de compras:"
            foreach articulo $lista_compras {
                puts " - $articulo"
            }
        }
        4 {
            # Abre el archivo de texto para escritura
            set fp [open $archivo_texto w]

            # Escribe la lista de compras en el archivo de texto
            foreach articulo $lista_compras {
                puts $fp "$articulo"
            }

            # Cierra el archivo de texto
            close $fp

            # Imprime un mensaje de confirmación
            puts "\nLista de compras guardada en el archivo de texto:", $archivo_texto
        }
        5 {
            # Abre el archivo de texto para lectura
            set fp [open $archivo_texto r]

            # Lee las líneas del archivo de texto
            set lineas [split [read $fp] "\n"]

            # Cierra el archivo de texto
            close $fp

            # Limpia la lista de compras actual
            set lista_compras {}

            # Agrega las líneas del archivo de texto a la lista de compras
            foreach linea $lineas {
                lappend lista_compras $linea
            }

            # Imprime un mensaje de confirmación
            puts "\nLista de compras cargada desde el archivo de texto:", $archivo_texto
        }
        6 {
            # Sale del programa
            break
        }
        default {
            # Imprime un mensaje de error
            puts "\nOpción inválida. Inténtelo nuevamente."
        }
    }
}

# Después de salir del bucle principal, imprime un mensaje de despedida
puts "\n¡Gracias por usar el programa!"
```

Explicación del código:

* El programa comienza con la información de encabezado, que incluye el título del programa y una línea divisoria.
* Se definen las variables `lista_compras` y `archivo_texto` para almacenar la lista de compras y el nombre del archivo de texto, respectivamente.
* Se definen dos funciones: `agregar_articulo` y `eliminar_articulo`, que agregan y eliminan un artículo de la lista de compras, respectivamente.
* El bucle principal del programa presenta las opciones al usuario y ejecuta la acción correspondiente a la opción seleccionada.
* Las opciones disponibles son:
    * Agregar un artículo a la lista de compras.
    * Eliminar un artículo de la lista de compras.
    * Imprimir la lista de compras en la consola.
    * Guardar la lista de compras en un archivo de texto.
    * Cargar la lista de compras desde un archivo de texto.
    * Salir del programa.
* El programa utiliza la instrucción `switch` para ejecutar la acción correspondiente a la opción seleccionada por el usuario.
* El programa finaliza cuando el usuario selecciona la opción de salir.
* Después de salir del bucle principal, el programa imprime un mensaje de despedida.