```tcl
#Procedimiento para encontrar el máximo común divisor (MCD) de dos números
proc mcd {x y} {
    while {$y != 0} {
        set tmp $y
        set y [expr {$x % $y}]
        set x $tmp
    }
    return $x
}

#Main
set n1 123
set n2 456
set mcd [mcd $n1 $n2]
puts "El máximo común divisor de $n1 y $n2 es $mcd"

# -----------------------------------------------------------------------

#Procedimiento para generar una secuencia de Fibonacci de longitud n
proc fibonacci {n} {
    if {$n <= 2} {
        return 1
    }
    set fib {1 1}
    while {[llength $fib] < $n} {
        set next [expr {$fib[end-1] + $fib[end-2]}]
        lappend fib $next
    }
    return [lrange $fib 0 end-1]
}

#Main
set n 10
set fib [fibonacci $n]
puts "La secuencia de Fibonacci de longitud $n es: [join $fib " "]"

# -----------------------------------------------------------------------

#Procedimiento para ordenar una lista de números en orden ascendente
proc ordenar {lista} {
    if {[llength $lista] < 2} {
        return $lista
    }
    set pivote $lista[0]
    set menores {}
    set mayores {}
    foreach num $lista {
        if {$num < $pivote} {
            lappend menores $num
        } elseif {$num > $pivote} {
            lappend mayores $num
        }
    }
    set ordenados [ordenar $menores][list $pivote][ordenar $mayores]
    return $ordenados
}

#Main
set lista {3 1 4 2 5}
set lista_ordenada [ordenar $lista]
puts "Lista original: [join $lista " "]
Lista ordenada: [join $lista_ordenada " "]"
```


Explicación del código:

**Procedimiento mcd**:
Este procedimiento calcula el máximo común divisor (MCD) de dos números utilizando el algoritmo de Euclides.

**Procedimiento fibonacci**:
Este procedimiento genera una secuencia de Fibonacci de longitud n. Una secuencia de Fibonacci es una serie de números en la que cada número es la suma de los dos números anteriores.

**Procedimiento ordenar**:
Este procedimiento ordena una lista de números en orden ascendente utilizando el algoritmo de clasificación por selección. Este algoritmo encuentra el elemento más pequeño de la lista y lo coloca al principio de la lista, luego encuentra el segundo elemento más pequeño y lo coloca después del primer elemento, y así sucesivamente.