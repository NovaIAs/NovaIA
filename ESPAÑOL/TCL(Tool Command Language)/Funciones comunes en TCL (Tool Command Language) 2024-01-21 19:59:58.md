```tcl
# Definición de una función para saludar
proc saludar {nombre} {
    puts "Hola, $nombre!"
}

# Definición de una función para sumar dos números
proc sumar {número1 número2} {
    expr {$número1 + $número2}
}

# Definición de una función para crear una lista de números
proc crear_lista {número1 número2 ...} {
    return [list $número1 $número2 $arg]
}

# Definición de un procedimiento para imprimir los elementos de una lista
proc imprimir_lista {lista} {
    foreach elemento $lista {
        puts $elemento
    }
}

# Definición de un procedimiento para crear un diccionario
proc crear_diccionario {clave1 valor1 clave2 valor2 ...} {
    set diccionario {}
    foreach clave valor $arg {
        set diccionario($clave) $valor
    }
    return $diccionario
}

# Definición de un procedimiento para imprimir los elementos de un diccionario
proc imprimir_diccionario {diccionario} {
    foreach clave valor $diccionario {
        puts "$clave: $valor"
    }
}

# Definición de una función para comparar dos cadenas
proc comparar_cadenas {cadena1 cadena2} {
    if {$cadena1 == $cadena2} {
        return 0
    } elseif {$cadena1 < $cadena2} {
        return -1
    } else {
        return 1
    }
}

# Definición de una función para ordenar una lista de cadenas
proc ordenar_lista {lista} {
    set lista_ordenada [lsort -index 0 $lista]
    return $lista_ordenada
}

# Definición de una función para buscar un elemento en una lista
proc buscar_elemento {lista elemento} {
    foreach elemento_lista $lista {
        if {$elemento_lista == $elemento} {
            return 1
        }
    }
    return 0
}

# Definición de una función para eliminar un elemento de una lista
proc eliminar_elemento {lista elemento} {
    set lista_nueva [lsearch -exact -index 0 $lista $elemento]
    return [lreplace $lista $lista_nueva 0 0]
}

# Definición de una función para insertar un elemento en una lista
proc insertar_elemento {lista elemento índice} {
    return [lset lista $índice $elemento]
}

# Definición de una función para crear una expresión regular
proc crear_expresión_regular {patrón} {
    return [regexp $patrón]
}

# Definición de una función para buscar una cadena que coincida con una expresión regular
proc buscar_cadena {cadena expresión_regular} {
    if {[regexp $expresión_regular $cadena]} {
        return 1
    } else {
        return 0
    }
}

# Definición de una función para reemplazar una cadena en una cadena
proc reemplazar_cadena {cadena cadena_a_reemplazar cadena_de_reemplazo} {
    return [string map $cadena_a_reemplazar $cadena_de_reemplazo $cadena]
}

# Definición de una función para convertir una cadena a minúsculas
proc minusculas {cadena} {
    return [string tolower $cadena]
}

# Definición de una función para convertir una cadena a mayúsculas
proc mayúsculas {cadena} {
    return [string toupper $cadena]
}

# Definición de una función para recortar los espacios en blanco de una cadena
proc recortar {cadena} {
    return [string trim $cadena]
}

# Definición de una función para dividir una cadena en una lista de palabras
proc dividir_cadena {cadena} {
    return [split $cadena " "]
}

# Definición de una función para concatenar dos cadenas
proc concatenar {cadena1 cadena2} {
    return [concat $cadena1 $cadena2]
}

# Definición de una función para obtener la longitud de una cadena
proc longitud {cadena} {
    return [string length $cadena]
}

# Definición de una función para obtener la subcadena de una cadena
proc subcadena {cadena índice_inicial índice_final} {
    return [string range $cadena $índice_inicial $índice_final]
}

# Definición de una función para obtener el índice de la primera ocurrencia de una cadena en otra cadena
proc índice_de {cadena cadena_a_buscar} {
    return [string first $cadena_a_buscar $cadena]