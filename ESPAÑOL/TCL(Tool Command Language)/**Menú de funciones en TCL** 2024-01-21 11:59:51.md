```tcl
# Definimos una función llamada "saludar" que recibe un nombre como parámetro y devuelve un saludo.
proc saludar {nombre} {
    # Utilizamos la palabra reservada "return" para devolver el saludo.
    return "Hola, $nombre! ¿Cómo estás?"
}

# Definimos una función llamada "despedir" que recibe un nombre como parámetro y devuelve una despedida.
proc despedir {nombre} {
    # Utilizamos la palabra reservada "return" para devolver la despedida.
    return "Adiós, $nombre! Que tengas un buen día."
}

# Definimos una función llamada "calcularAreaTriangulo" que recibe tres números como parámetros y devuelve el área de un triángulo.
proc calcularAreaTriangulo {base altura} {
    # Utilizamos la palabra reservada "return" para devolver el área del triángulo.
    return ($base * $altura) / 2
}

# Definimos una función llamada "calcularVolumenEsfera" que recibe un número como parámetro y devuelve el volumen de una esfera.
proc calcularVolumenEsfera {radio} {
    # Utilizamos la palabra reservada "return" para devolver el volumen de la esfera.
    return (4/3) * pi * ($radio * $radio * $radio)
}

# Definimos una función llamada "mostrarMenu" que no recibe ningún parámetro y muestra un menú en pantalla.
proc mostrarMenu {} {
    # Utilizamos la palabra reservada "puts" para mostrar un mensaje en pantalla.
    puts "**************************************"
    puts "* 1. Saludar                         *"
    puts "* 2. Despedir                        *"
    puts "* 3. Calcular área de un triángulo    *"
    puts "* 4. Calcular volumen de una esfera    *"
    puts "* 5. Salir                           *"
    puts "**************************************"
    puts "Elige una opción: "
}

# Definimos una variable llamada "opcion" que inicialmente está vacía.
set opcion ""

# Mientras la opción no sea 5 (salir), seguimos mostrando el menú y preguntando al usuario por una opción.
while {$opcion != 5} {
    # Mostramos el menú en pantalla.
    mostrarMenu

    # Leemos la opción introducida por el usuario.
    set opcion [gets stdin]

    # Convertimos la opción a un número entero.
    set opcion [expr {$opcion}]

    # Según la opción elegida, ejecutamos la función correspondiente.
    switch $opcion {
        1 {
            # Pedimos al usuario que introduzca su nombre.
            puts "Introduce tu nombre: "
            set nombre [gets stdin]

            # Saludamos al usuario utilizando la función "saludar".
            puts [saludar $nombre]
        }
        2 {
            # Pedimos al usuario que introduzca su nombre.
            puts "Introduce tu nombre: "
            set nombre [gets stdin]

            # Despedimos al usuario utilizando la función "despedir".
            puts [despedir $nombre]
        }
        3 {
            # Pedimos al usuario que introduzca la base y la altura del triángulo.
            puts "Introduce la base del triángulo: "
            set base [gets stdin]

            puts "Introduce la altura del triángulo: "
            set altura [gets stdin]

            # Calculamos el área del triángulo utilizando la función "calcularAreaTriangulo".
            set area [calcularAreaTriangulo $base $altura]

            # Mostramos el área del triángulo.
            puts "El área del triángulo es $area"
        }
        4 {
            # Pedimos al usuario que introduzca el radio de la esfera.
            puts "Introduce el radio de la esfera: "
            set radio [gets stdin]

            # Calculamos el volumen de la esfera utilizando la función "calcularVolumenEsfera".
            set volumen [calcularVolumenEsfera $radio]

            # Mostramos el volumen de la esfera.
            puts "El volumen de la esfera es $volumen"
        }
        5 {
            # Salimos del bucle.
            break
        }
        default {
            # Si el usuario introduce una opción incorrecta, mostramos un mensaje de error.
            puts "Opción incorrecta. Introduce un número del 1 al 5."
        }
    }
}

# Mostramos un mensaje de despedida al usuario.
puts "Hasta pronto!"
```

Explicación del código:

* Definimos varias funciones que realizan diferentes tareas, como saludar a un usuario, despedirlo, calcular el área de un triángulo y el volumen de una esfera.
* Definimos una función llamada "mostrarMenu" que muestra un menú en pantalla y permite al usuario elegir una opción.
* Utilizamos un bucle "while" para seguir mostrando el menú y preguntando al usuario por una opción hasta que el usuario elija salir.
* Según la opción elegida por el usuario, ejecutamos la función correspondiente.
* Si el usuario introduce una opción incorrecta, mostramos un mensaje de error.
* Cuando el usuario elige salir, salimos del bucle y mostramos un mensaje de despedida.