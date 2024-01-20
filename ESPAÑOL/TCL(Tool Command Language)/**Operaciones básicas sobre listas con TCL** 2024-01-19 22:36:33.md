```tcl
# Crear una lista de valores
valores = {1, 2, 3, 4, 5}

# Definir una función para calcular la suma de una lista
proc suma {lista} {
    set acumulador 0
    foreach valor en $lista {
        set acumulador [expr {$acumulador + $valor}]
    }
    return $acumulador
}

# Imprimir la suma de la lista de valores
puts [suma $valores]

# Definir una función para calcular el promedio de una lista
proc promedio {lista} {
    set suma [suma $lista]
    set longitud [llength $lista]
    return [expr {$suma / $longitud}]
}

# Imprimir el promedio de la lista de valores
puts [promedio $valores]

# Definir una función para encontrar el valor máximo de una lista
proc max {lista} {
    set maximo [lindex $lista 0]
    foreach valor en $lista {
        if {$valor > $maximo} {
            set maximo $valor
        }
    }
    return $maximo
}

# Imprimir el valor máximo de la lista de valores
puts [max $valores]

# Definir una función para encontrar el valor mínimo de una lista
proc min {lista} {
    set minimo [lindex $lista 0]
    foreach valor en $lista {
        if {$valor < $minimo} {
            set minimo $valor
        }
    }
    return $minimo
}

# Imprimir el valor mínimo de la lista de valores
puts [min $valores]
```

Explicación:

* La primera línea crea una lista de valores (1, 2, 3, 4, 5).
* La segunda línea define una función llamada `suma` que toma una lista como argumento y calcula su suma.
* La tercera línea imprime la suma de la lista de valores.
* La cuarta línea define una función llamada `promedio` que toma una lista como argumento y calcula su promedio.
* La quinta línea imprime el promedio de la lista de valores.
* La sexta línea define una función llamada `max` que toma una lista como argumento y encuentra su valor máximo.
* La séptima línea imprime el valor máximo de la lista de valores.
* La octava línea define una función llamada `min` que toma una lista como argumento y encuentra su valor mínimo.
* La novena línea imprime el valor mínimo de la lista de valores.