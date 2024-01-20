```tcl
# Código complejo en TCL (Tool Command Language)

# Variables
variable1 10
variable2 20
variable3 30

# Listas
lista1 {uno dos tres cuatro cinco}
lista2 {rojo azul verde amarillo naranja}

# Diccionarios
diccionario1 {
    uno uno
    dos dos
    tres tres
}

diccionario2 {
    rojo rojo
    azul azul
    verde verde
}

# Funciones
proc suma {a b} {
    expr {$a + $b}
}

proc resta {a b} {
    expr {$a - $b}
}

proc multiplicación {a b} {
    expr {$a * $b}
}

proc división {a b} {
    expr {$a / $b}
}

# If-Else
if {$variable1 > $variable2} {
    puts "La variable1 es mayor que la variable2"
} elseif {$variable1 == $variable2} {
    puts "La variable1 es igual a la variable2"
} else {
    puts "La variable1 es menor que la variable2"
}

# Switch-Case
switch -exact -- $variable3 {
    10 {
        puts "La variable3 es igual a 10"
        break
    }
    20 {
        puts "La variable3 es igual a 20"
        break
    }
    30 {
        puts "La variable3 es igual a 30"
        break
    }
    default {
        puts "La variable3 no es igual a 10, 20 ni 30"
        break
    }
}

# For-Each
foreach elemento $lista1 {
    puts "Elemento de la lista1: $elemento"
}

foreach (color valor) $diccionario2 {
    puts "Color: $color, Valor: $valor"
}

# While
while {$variable1 < $variable2} {
    puts "Variable1: $variable1"
    incr variable1
}

# Repeat
repeat 10 {
    puts "Repetición: $tcl_repeat"
}

# Exit
exit

# Explicación
# Este código es un ejemplo de código complejo en TCL (Tool Command Language). Incluye variables, listas, diccionarios, funciones, estructuras de control y bucles.
# El código también incluye comentarios para explicar cada sección del código.
# Este código se puede utilizar para aprender TCL o para desarrollar aplicaciones complejas en TCL.
```