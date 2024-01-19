```tcl
# Definir una función para saludar a alguien
proc saludar {nombre} {
    puts "Hola, $nombre!"
}

# Crear una lista de nombres
nombres = {Juan, María, Pedro, Ana}

# Iterar sobre la lista de nombres y saludar a cada persona
foreach nombre $nombres {
    saludar $nombre
}

# Definir una función para calcular el factorial de un número
proc factorial {número} {
    if {$número <= 1} {
        return 1
    }
    return $número * factorial [expr {$número - 1}]
}

# Calcular el factorial de 5
factorial 5

# Definir una función para generar una lista de números aleatorios
proc generar_lista_aleatorios {longitud} {
    set lista {}
    for {set i 0} {$i < $longitud} {incr i} {
        lappend lista [expr {rand() * 100}]
    }
    return $lista
}

# Generar una lista de 10 números aleatorios
generar_lista_aleatorios 10

# Definir una función para ordenar una lista de números de manera ascendente
proc ordenar_ascendente {lista} {
    return [lsort -increasing $lista]
}

# Ordenar una lista de números de manera ascendente
ordenar_ascendente {1, 3, 5, 2, 4}

# Definir una función para ordenar una lista de números de manera descendente
proc ordenar_descendente {lista} {
    return [lsort -decreasing $lista]
}

# Ordenar una lista de números de manera descendente
ordenar_descendente {1, 3, 5, 2, 4}

# Definir una función para buscar un elemento en una lista
proc buscar_elemento {lista elemento} {
    for {set i 0} {$i < [llength $lista]} {incr i} {
        if {$lista($i) == $elemento} {
            return $i
        }
    }
    return -1
}

# Buscar un elemento en una lista
buscar_elemento {1, 3, 5, 2, 4} 3

# Definir una función para crear un diccionario
proc crear_diccionario {claves valores} {
    set diccionario {}
    for {set i 0} {$i < [llength $claves]} {incr i} {
        dict set diccionario [lindex $claves $i] [lindex $valores $i]
    }
    return $diccionario
}

# Crear un diccionario
crear_diccionario {nombre edad} {Juan 20 María 25 Pedro 30}

# Obtener el valor de una clave en un diccionario
dict get $diccionario Juan

# Agregar un nuevo elemento a un diccionario
dict set $diccionario Ana 35

# Eliminar un elemento de un diccionario
dict unset $diccionario Pedro

# Iterar sobre las claves y valores de un diccionario
foreach clave valor [dict keys $diccionario] [dict values $diccionario] {
    puts "$clave: $valor"
}
```

Este código TCL realiza una serie de tareas complejas, incluyendo:

* Definición de funciones para saludar a alguien, calcular el factorial de un número, generar una lista de números aleatorios, ordenar una lista de números, buscar un elemento en una lista, crear un diccionario y obtener, agregar y eliminar elementos de un diccionario.
* Uso de estructuras de control como bucles `foreach` y `for`, condicionales `if` y declaraciones `return` para controlar el flujo del programa.
* Utilización de la sentencia `puts` para mostrar mensajes en la consola.
* Utilización de la sentencia `lappend` para añadir elementos a una lista.
* Utilización de la sentencia `lsort` para ordenar una lista.
* Utilización de la sentencia `dict set` para crear un diccionario y asignar valores a sus claves.
* Utilización de la sentencia `dict get` para obtener el valor de una clave en un diccionario.
* Utilización de la sentencia `dict unset` para eliminar un elemento de un diccionario.

Este código es bastante complejo y cubre una amplia gama de funcionalidades de TCL. Es un buen ejemplo de cómo utilizar TCL para realizar tareas complejas de manera eficiente y concisa.