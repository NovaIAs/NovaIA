```tcl
# Crear una lista de números
lista = [1 2 3 4 5 6 7 8 9 10]

# Definir una función para calcular la suma de los números en una lista
proc suma {lista} {
    set suma 0
    foreach num $lista {
        set suma [expr {$suma + $num}]
    }
    return $suma
}

# Definir una función para calcular el promedio de los números en una lista
proc promedio {lista} {
    set suma [suma $lista]
    set longitud [llength $lista]
    return [expr {$suma / $longitud}]
}

# Definir una función para encontrar el número máximo en una lista
proc max {lista} {
    set maximo $lista(0)
    foreach num $lista {
        if {$num > $maximo} {
            set maximo $num
        }
    }
    return $maximo
}

# Definir una función para encontrar el número mínimo en una lista
proc min {lista} {
    set minimo $lista(0)
    foreach num $lista {
        if {$num < $minimo} {
            set minimo $num
        }
    }
    return $minimo
}

# Definir una función para imprimir los resultados
proc imprimir_resultados {lista suma promedio maximo minimo} {
    puts "Lista de números:"
    foreach num $lista {
        puts "$num"
    }
    puts "Suma de los números: $suma"
    puts "Promedio de los números: $promedio"
    puts "Número máximo: $maximo"
    puts "Número mínimo: $minimo"
}

# Llamar a las funciones para calcular los resultados
set suma [suma $lista]
set promedio [promedio $lista]
set maximo [max $lista]
set minimo [min $lista]

# Imprimir los resultados
imprimir_resultados $lista $suma $promedio $maximo $minimo
```

Este código en TCL realiza las siguientes operaciones:

1. Crea una lista de números llamada `lista`.
2. Define una función llamada `suma` que calcula la suma de los números en una lista.
3. Define una función llamada `promedio` que calcula el promedio de los números en una lista.
4. Define una función llamada `max` que encuentra el número máximo en una lista.
5. Define una función llamada `min` que encuentra el número mínimo en una lista.
6. Define una función llamada `imprimir_resultados` que imprime los resultados de las operaciones anteriores.
7. Llama a las funciones para calcular los resultados y los almacena en las variables `suma`, `promedio`, `maximo` y `minimo`.
8. Finalmente, llama a la función `imprimir_resultados` para mostrar los resultados en pantalla.