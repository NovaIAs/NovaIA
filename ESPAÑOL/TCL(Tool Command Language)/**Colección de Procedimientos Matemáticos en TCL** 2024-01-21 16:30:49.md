```tcl
# Procedimiento para calcular el factorial de un número
proc factorial {n} {
    if {$n == 0} {
        return 1
    } else {
        return [expr {$n * [factorial [expr {$n - 1}]]}]
    }
}

# Procedimiento para imprimir un patrón de asteriscos
proc imprimir_patron {n} {
    for {set i 0} {$i < $n} {incr i} {
        for {set j 0} {$j ≤ $i} {incr j} {
            puts -nonewline "*"
        }
        puts ""
    }
}

# Procedimiento para encontrar el máximo común divisor de dos números
proc mcd {a b} {
    while {$b != 0} {
        set temp [expr {$a % $b}]
        set a $b
        set b $temp
    }
    return $a
}

# Procedimiento para encontrar el mínimo común múltiplo de dos números
proc mcm {a b} {
    return [expr {$a * $b / [mcd $a $b]}]
}

# Procedimiento para comprobar si un número es primo
proc es_primo {n} {
    if {$n <= 1} {
        return 0
    }
    for {set i 2} {$i < $n} {incr i} {
        if {[expr {$n % $i}] == 0} {
            return 0
        }
    }
    return 1
}

# Procedimiento para encontrar la serie de Fibonacci hasta un número dado
proc fibonacci {n} {
    set secuencia {0 1}
    while {[llength $secuencia] < $n} {
        set next [expr {[lindex $secuencia end - 2] + [lindex $secuencia end - 1]}]
        lappend secuencia $next
    }
    return $secuencia
}

# Procedimiento para ordenar una lista de números
proc ordenar {lista} {
    set ordenada {}
    while {[llength $lista] > 0} {
        set min [lindex $lista 0]
        set min_index 0
        for {set i 1} {[llength $lista] > $i} {incr i} {
            if {[lindex $lista $i] < $min} {
                set min [lindex $lista $i]
                set min_index $i
            }
        }
        lappend ordenada $min
        lset lista $min_index {}
    }
    return $ordenada
}
```

Este código contiene una colección de procedimientos útiles para realizar cálculos matemáticos y operaciones de procesamiento de listas:

* `factorial`: Calcula el factorial de un número.
* `imprimir_patron`: Imprime un patrón de asteriscos en forma de triángulo rectángulo.
* `mcd`: Calcula el máximo común divisor de dos números.
* `mcm`: Calcula el mínimo común múltiplo de dos números.
* `es_primo`: Comprueba si un número es primo.
* `fibonacci`: Calcula la serie de Fibonacci hasta un número dado.
* `ordenar`: Ordena una lista de números en orden ascendente.

Todos estos procedimientos están implementados utilizando la sintaxis de TCL, que es un lenguaje de programación de alto nivel con una sintaxis concisa y fácil de leer. Las funciones están definidas utilizando la palabra clave `proc`, y se pueden llamar desde otras funciones o desde la línea de comandos.

Este código es un buen ejemplo de cómo TCL puede utilizarse para realizar cálculos matemáticos complejos y operaciones de procesamiento de listas de forma eficiente.