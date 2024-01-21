```tcl
# Se define una función que recibe una lista de números y devuelve la suma de ellos.
proc sumar {lista} {
    set suma 0
    foreach numero $lista {
        set suma [expr {$suma + $numero}]
    }
    return $suma
}

# Se define una función que recibe una lista de números y devuelve el promedio de ellos.
proc promedio {lista} {
    set suma [sumar $lista]
    set cantidad [llength $lista]
    return [expr {$suma / $cantidad}]
}

# Se define una función que recibe una lista de números y devuelve la mediana de ellos.
proc mediana {lista} {
    # Se ordena la lista de números en orden ascendente.
    set lista_ordenada [lsort $lista]

    # Se obtiene la posición del elemento central de la lista ordenada.
    set posicion_central [expr {[llength $lista_ordenada] / 2}]

    # Si la longitud de la lista es par, se devuelve el promedio de los dos elementos centrales.
    if {[llength $lista_ordenada] % 2 == 0} {
        set mediana [expr {([lindex $lista_ordenada $posicion_central] + [lindex $lista_ordenada [expr {$posicion_central + 1}]]) / 2}]
    }
    # Si la longitud de la lista es impar, se devuelve el elemento central.
    else {
        set mediana [lindex $lista_ordenada $posicion_central]
    }

    return $mediana
}

# Se define una función que recibe una lista de números y devuelve la moda de ellos.
proc moda {lista} {
    # Se cuenta el número de veces que aparece cada elemento en la lista.
    set recuentos [dict create]
    foreach numero $lista {
        if {[dict exists $recuentos $numero]} {
            set recuentos($numero) [incr recuentos($numero)]
        } else {
            set recuentos($numero) 1
        }
    }

    # Se obtiene el elemento con el mayor número de veces que aparece en la lista.
    set moda ""
    set max_recuento 0
    foreach elemento [dict keys $recuentos] {
        set recuento $recuentos($elemento)
        if {$recuento > $max_recuento} {
            set moda $elemento
            set max_recuento $recuento
        }
    }

    return $moda
}

# Se pide al usuario que introduzca una lista de números.
puts "Introduzca una lista de números, separados por espacios:"
set lista [gets stdin]

# Se llama a las funciones para calcular la suma, el promedio, la mediana y la moda de la lista de números.
set suma [sumar $lista]
set promedio [promedio $lista]
set mediana [mediana $lista]
set moda [moda $lista]

# Se muestran los resultados al usuario.
puts "La suma de los números es: $suma"
puts "El promedio de los números es: $promedio"
puts "La mediana de los números es: $mediana"
puts "La moda de los números es: $moda"
```

Este código es un ejemplo de programa Tcl que calcula la suma, el promedio, la mediana y la moda de una lista de números introducida por el usuario.

El programa define cuatro funciones:

* `sumar`: Esta función recibe una lista de números y devuelve la suma de ellos.
* `promedio`: Esta función recibe una lista de números y devuelve el promedio de ellos.
* `mediana`: Esta función recibe una lista de números y devuelve la mediana de ellos.
* `moda`: Esta función recibe una lista de números y devuelve la moda de ellos.

El programa pide al usuario que introduzca una lista de números, separados por espacios.

A continuación, el programa llama a las funciones para calcular la suma, el promedio, la mediana y la moda de la lista de números introducida por el usuario.

Por último, el programa muestra los resultados al usuario.