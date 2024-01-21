```tcl
# Procedimiento para calcular el área de un triángulo.
proc area_triangulo {base altura} {
    return [expr {0.5 * $base * $altura}]
}

# Procedimiento para calcular el área de un círculo.
proc area_circulo {radio} {
    return [expr {pi * $radio^2}]
}

# Procedimiento para calcular el área de un rectángulo.
proc area_rectangulo {largo ancho} {
    return [expr {$largo * $ancho}]
}

# Procedimiento para calcular el área de un cuadrado.
proc area_cuadrado {lado} {
    return [expr {$lado^2}]
}

# Procedimiento principal.
proc main {} {
    # Mostrar un mensaje de bienvenida.
    puts "Calculador de áreas"

    # Obtener el tipo de figura del usuario.
    puts "Elija una figura:"
    puts "1. Triángulo"
    puts "2. Círculo"
    puts "3. Rectángulo"
    puts "4. Cuadrado"
    set figura [gets]

    # Obtener los parámetros de la figura del usuario.
    switch $figura {
        1 {
            puts "Ingrese la base del triángulo:"
            set base [gets]
            puts "Ingrese la altura del triángulo:"
            set altura [gets]
        }
        2 {
            puts "Ingrese el radio del círculo:"
            set radio [gets]
        }
        3 {
            puts "Ingrese el largo del rectángulo:"
            set largo [gets]
            puts "Ingrese el ancho del rectángulo:"
            set ancho [gets]
        }
        4 {
            puts "Ingrese el lado del cuadrado:"
            set lado [gets]
        }
        default {
            puts " figura inválida. Por favor, elija una figura del 1 al 4."
            exit 1
        }
    }

    # Calcular el área de la figura.
    set area 0
    switch $figura {
        1 {
            set area [area_triangulo $base $altura]
        }
        2 {
            set area [area_circulo $radio]
        }
        3 {
            set area [area_rectangulo $largo $ancho]
        }
        4 {
            set area [area_cuadrado $lado]
        }
    }

    # Mostrar el área de la figura.
    puts "El área de la figura es $area."
}

# Llamar al procedimiento principal.
main
```

Explicación del código:

* El código comienza definiendo cuatro procedimientos: `area_triangulo`, `area_circulo`, `area_rectangulo` y `area_cuadrado`. Cada uno de estos procedimientos calcula el área de una figura geométrica diferente.
* El procedimiento `main` es el procedimiento principal del programa. Este procedimiento muestra un mensaje de bienvenida al usuario, obtiene el tipo de figura del usuario, obtiene los parámetros de la figura del usuario, calcula el área de la figura y muestra el área de la figura al usuario.
* El procedimiento `switch` se utiliza para seleccionar el procedimiento adecuado para calcular el área de la figura.
* El procedimiento `gets` se utiliza para obtener la entrada del usuario.
* El procedimiento `expr` se utiliza para evaluar expresiones matemáticas.
* El procedimiento `puts` se utiliza para mostrar mensajes en la consola.