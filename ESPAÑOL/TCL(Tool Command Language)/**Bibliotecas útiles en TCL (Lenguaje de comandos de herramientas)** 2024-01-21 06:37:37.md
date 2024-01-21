```tcl
# Crear una función que reciba un número y devuelva su factorial.
proc factorial {num} {
    if {[string is integer $num] && [expr {$num >= 0}]} {
        if {[expr {$num == 0} || {num == 1}]} {
            return 1
        } else {
            return [expr {$num * [factorial [expr {$num - 1}]]}]
        }
    } else {
        return -1  # Número no válido
    }
}

# Crear una función que reciba dos listas y devuelva una lista con los elementos comunes a ambas listas.
proc intersect {list1 list2} {
    set common {}
    foreach element [list $list1] {
        if {[lsearch -exact $list2 $element] != -1} {
            lappend common $element
        }
    }
    return $common
}

# Crear una función que reciba un número y devuelva una lista con sus divisores.
proc divisors {num} {
    set divisors {}
    for {set i 1} {[expr {$i * i} <= $num]} {incr i} {
        if {[expr {$num % $i == 0}]} {
            lappend divisors $i
            if {[expr {$num / $i} != $i]} {
                lappend divisors [expr {$num / $i}]
            }
        }
    }
    return $divisors
}

# Crear una función que reciba una lista de números y devuelva el máximo común divisor de todos los números de la lista.
proc gcd {list} {
    if {[llength $list] == 0} {
        return -1  # Lista vacía
    } elseif {[llength $list] == 1} {
        return [lindex $list 0]
    } else {
        set gcd [llength $list]
        foreach num $list {
            set gcd [expr {$gcd `gcd` $num}]
        }
        return $gcd
    }
}

# Crear una función que reciba una cadena y devuelva una cadena con las letras en orden alfabético.
proc sort_letters {string} {
    set sorted [lsort -real [split $string ""]]
    return [join $sorted ""]
}

# Crear una función que reciba una lista de palabras y devuelva una lista con las palabras más largas.
proc longest_words {list} {
    set max_length 0
    set longest_words {}
    foreach word $list {
        set length [string length $word]
        if {[expr {$length > $max_length}]} {
            set max_length $length
            set longest_words {$word}
        } elseif {[expr {$length == $max_length}]} {
            lappend longest_words $word
        }
    }
    return $longest_words
}

# Crear una función que reciba una lista de números y devuelva el valor medio de todos los números de la lista.
proc average {list} {
    if {[llength $list] == 0} {
        return -1  # Lista vacía
    } else {
        set sum 0
        foreach num $list {
            incr sum $num
        }
        return [expr {$sum / [llength $list]}]
    }
}

# Crear una función que reciba una lista de palabras y devuelva el valor más frecuente en la lista.
proc most_frequent {list} {
    set counts {}
    foreach word $list {
        incr counts($word)
    }
    set max_count 0
    set most_frequent ""
    foreach word [lsort -integer -decreasing [lkeys $counts]] {
        set count [lindex $counts $word]
        if {[expr {$count > $max_count}]} {
            set max_count $count
            set most_frequent $word
        }
    }
    return $most_frequent
}

# Crear una función que reciba dos números y devuelva el mínimo común múltiplo de ambos números.
proc lcm {num1 num2} {
    set gcd [expr {$num1 `gcd` $num2}]
    return [expr {($num1 * $num2) / $gcd}]
}

```

Estas son algunas funciones útiles que puedes usar para realizar tareas comunes en Tcl.