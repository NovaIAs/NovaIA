```tcl
# Procedimiento para calcular el factorial de un número natural
proc factorial {n} {
    if {$n <= 1} {
        return 1
    } else {
        return [expr {$n * [factorial [expr {$n-1}]]}]
    }
}

# Procedimiento para imprimir una tabla de multiplicar
proc tabla_multiplicar {n} {
    puts "Tabla de multiplicar del número $n:"
    for {set i 1} {$i <= 10} {incr i} {
        puts "[expr {$n * $i}]"
    }
}

# Procedimiento para buscar un elemento en una lista
proc buscar_elemento {lista elemento} {
    foreach item $lista {
        if {$item == $elemento} {
            return 1
        }
    }
    return 0
}

# Procedimiento para ordenar una lista
proc ordenar_lista {lista} {
    set ordenada {}
    while {[llength $lista] > 0} {
        set min [lrange $lista 0 0]
        foreach item [lrange $lista 1 end] {
            if {$item < $min} {
                set min $item
            }
        }
        lappend ordenada $min
        set lista [lreplace $lista 0 [lsearch -exact $lista $min] ""]
    }
    return $ordenada
}

# Procedimiento para generar una lista de números primos
proc generar_primos {n} {
    set primos {}
    for {set i 2} {$i <= $n} {incr i} {
        set es_primo 1
        for {set j 2} {$j < $i} {incr j} {
            if {[expr {$i % $j}] == 0} {
                set es_primo 0
                break
            }
        }
        if {$es_primo} {
            lappend primos $i
        }
    }
    return $primos
}

# Procedimiento para convertir un número decimal a binario
proc decimal_a_binario {n} {
    if {$n == 0} {
        return ""
    }
    return [decimal_a_binario [expr {$n / 2}]] [expr {$n mod 2}]
}

# Procedimiento para convertir un número binario a decimal
proc binario_a_decimal {n} {
    set decimal 0
    set potencia 1
    while {[string length $n] > 0} {
        set decimal [expr {$decimal + [string first $n] * $potencia}]
        set n [string range $n 1 end]
        set potencia [expr {$potencia * 2}]
    }
    return $decimal
}

# Procedimiento para calcular el máximo común divisor de dos números naturales
proc mcd {a b} {
    while {$b > 0} {
        set temp $b
        set b [expr {$a % $b}]
        set a $temp
    }
    return $a
}

# Procedimiento para calcular el mínimo común múltiplo de dos números naturales
proc mcm {a b} {
    return [expr {$a * $b / [mcd $a $b]}]
}

# Procedimiento para encontrar el mayor número primo menor que un número dado
proc mayor_primo_menor_que {n} {
    set primo 2
    while {[expr {$primo * $primo}] <= $n} {
        if {[expr {$n % $primo}] == 0} {
            set n [expr {$n / $primo}]
        } else {
            set primo [expr {$primo + 1}]
        }
    }
    return $n
}

# Procedimiento para generar una lista de todos los divisores de un número natural
proc divisores {n} {
    set divisores {}
    for {set i 1} {$i <= $n} {incr i} {
        if {[expr {$n % $i}] == 0} {
            lappend divisores $i
        }
    }
    return $divisores
}
```

Este código complejo en TCL incluye una serie de procedimientos para realizar diversas operaciones matemáticas y de procesamiento de listas. Aquí hay una explicación de cada procedimiento:

* `factorial`: Calcula el factorial de un número natural.
* `tabla_multiplicar`: Imprime una tabla de multiplicar para un número dado.
* `buscar_elemento`: Busca un elemento en una lista y devuelve 1 si se encuentra, o 0 si no se encuentra.
* `ordenar_lista`: Ordena una lista en orden ascendente.
* `generar_primos`: Genera una lista de todos los números primos hasta un límite dado.
* `decimal_a_binario`: Convierte un número decimal a su representación binaria.
* `binario_a_decimal`: Convierte un número binario a su representación decimal.
* `mcd`: Calcula el máximo común divisor de dos números naturales.
* `mcm`: Calcula el mínimo común múltiplo de dos números naturales.
* `mayor_primo_menor_que`: Encuentra el mayor número primo menor que un número dado.
* `divisores`: Genera una lista de todos los divisores de un número natural.

Estos procedimientos pueden utilizarse para realizar diversas operaciones matemáticas y de procesamiento de listas, y pueden ser útiles en una amplia variedad de aplicaciones.