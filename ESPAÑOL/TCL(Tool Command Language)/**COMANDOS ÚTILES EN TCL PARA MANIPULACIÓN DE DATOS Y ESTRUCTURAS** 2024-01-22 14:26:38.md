```tcl
# Procedimiento para calcular el factorial de un número.
proc factorial {n} {
    if {$n == 0} {
        return 1
    }
    return [expr {$n * [factorial [expr {$n-1}]]}]
}

# Procedimiento para imprimir los números primos hasta un límite especificado.
proc imprimir_primos {limite} {
    set primos {}
    for {set i 2} {$i <= $limite} {incr i} {
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

# Procedimiento para generar una lista de números aleatorios.
proc generar_aleatorios {longitud limite} {
    set numeros {}
    for {set i 0} {$i < $longitud} {incr i} {
        lappend numeros [expr {int(rand()*$limite)}]
    }
    return $numeros
}

# Procedimiento para ordenar una lista de números.
proc ordenar_numeros {lista} {
    set ordenada {}
    while {[llength $lista] > 0} {
        set menor [lrange $lista 0 0]
        set indice_menor 0
        for {set i 1} {[llength $lista] > 0} {incr i} {
            if {[lrange $lista $i $i] < $menor} {
                set menor [lrange $lista $i $i]
                set indice_menor $i
            }
        }
        lappend ordenada $menor
        lset lista $indice_menor ""
    }
    return $ordenada
}

# Procedimiento para buscar un número en una lista ordenada.
proc buscar_numero {lista numero} {
    set inicio 0
    set fin [llength $lista] - 1
    while {$inicio <= $fin} {
        set medio [expr {($inicio + $fin) / 2}]
        if {[lrange $lista $medio $medio] == $numero} {
            return $medio
        } elseif {[lrange $lista $medio $medio] < $numero} {
            set inicio [expr {$medio + 1}]
        } else {
            set fin [expr {$medio - 1}]
        }
    }
    return -1
}

# Procedimiento para imprimir un menú de opciones.
proc imprimir_menu {opciones} {
    puts "\nMENÚ DE OPCIONES:"
    for {set i 0} {$i < [llength $opciones]} {incr i} {
        puts "[expr {$i+1}] $opciones($i)"
    }
    puts "[0] Salir"
    return
}

# Procedimiento para obtener una opción del usuario.
proc obtener_opcion {} {
    while {1} {
        puts "\nIngrese una opción:"
        set opcion [gets stdin]
        if {[string is integer $opcion]} {
            if {[expr {$opcion >= 0}] && [expr {$opcion <= [llength $opciones]} ]} {
                return $opcion
            }
        }
        puts "Opción inválida. Inténtelo de nuevo."
    }
}

# Procedimiento principal.
set opciones {"Calcular factorial", "Imprimir números primos", "Generar números aleatorios", "Ordenar números", "Buscar número en una lista", "Salir"}
while {1} {
    imprimir_menu $opciones
    set opcion [obtener_opcion]
    if {$opcion == 0} {
        break
    }
    switch $opcion {
        1 {
            puts "\nIngrese un número para calcular su factorial:"
            set numero [gets stdin]
            if {[string is integer $numero]} {
                puts "\nEl factorial de $numero es: [factorial $numero]"
            } else {
                puts "Número inválido. Inténtelo de nuevo."
            }
        }
        2 {
            puts "\nIngrese el límite para imprimir los números primos:"
            set limite [gets stdin]
            if {[string is integer $limite]} {
                puts "\nLos números primos hasta el límite $limite son:"
                puts [join [imprimir_primos $limite] " "]
            } else {
                puts "Límite inválido. Inténtelo de nuevo."