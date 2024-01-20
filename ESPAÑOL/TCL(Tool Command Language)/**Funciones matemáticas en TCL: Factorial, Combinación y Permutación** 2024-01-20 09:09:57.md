```tcl
# Procedimiento para calcular el factorial de un número
proc factorial {n} {
    if {$n == 0} {
        return 1
    } else {
        return [expr {$n * [factorial [expr {$n - 1}]]}]
    }
}

# Procedimiento para calcular la combinación de n elementos tomados de r en r
proc combinaciones {n r} {
    if {[expr {$n < $r}]} {
        return 0
    } else {
        return [expr {[factorial $n] / [factorial [expr {$n - $r}]] / [factorial $r]]}]
    }
}

# Procedimiento para calcular la permutación de n elementos tomados de r en r
proc permutaciones {n r} {
    if {[expr {$n < $r}]} {
        return 0
    } else {
        return [expr {[factorial $n] / [factorial [expr {$n - $r}]]}]
    }
}

# Función principal para mostrar los resultados de los procedimientos
proc main {} {
    # Mostrar los resultados del cálculo del factorial de 5
    puts [format "Factorial de 5: %d", [factorial 5]]

    # Mostrar los resultados del cálculo de la combinación de 10 elementos tomados de 5 en 5
    puts [format "Combinación de 10 elementos tomados de 5 en 5: %d", [combinaciones 10 5]]

    # Mostrar los resultados del cálculo de la permutación de 10 elementos tomados de 5 en 5
    puts [format "Permutación de 10 elementos tomados de 5 en 5: %d", [permutaciones 10 5]]
}

# Llamar a la función principal
main
```

Explicación del código:

* El procedimiento `factorial` calcula el factorial de un número utilizando recursión. Si el número es 0, devuelve 1. De lo contrario, devuelve el número multiplicado por el factorial del número disminuido en 1.
* El procedimiento `combinaciones` calcula la combinación de n elementos tomados de r en r utilizando la fórmula `n! / (n - r)! / r!`.
* El procedimiento `permutaciones` calcula la permutación de n elementos tomados de r en r utilizando la fórmula `n! / (n - r)!`.
* La función `main` llama a los procedimientos anteriores para calcular y mostrar los resultados del factorial de 5, la combinación de 10 elementos tomados de 5 en 5 y la permutación de 10 elementos tomados de 5 en 5.