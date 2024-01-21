```tcl
package require Tcl 8.6

# Define una estructura de datos para representar una persona
proc Persona {nombre apellido edad} {
    return [list $nombre $apellido $edad]
}

# Crea una lista de personas
set personas {
    [Persona "Juan" "García" 25]
    [Persona "María" "Pérez" 30]
    [Persona "Pedro" "López" 20]
}

# Define un procedimiento para imprimir la información de una persona
proc ImprimirPersona {persona} {
    puts "[lindex $persona 0] [lindex $persona 1] [lindex $persona 2]"
}

# Recorre la lista de personas e imprime la información de cada una
foreach persona $personas {
    ImprimirPersona $persona
}

# Define un procedimiento para calcular la edad promedio de las personas
proc EdadPromedio {personas} {
    set sumaEdad 0
    foreach persona $personas {
        set sumaEdad [expr {$sumaEdad + [lindex $persona 2]}]
    }
    return [expr {$sumaEdad / [llength $personas]}]
}

# Calcula la edad promedio de las personas
set edadPromedio [EdadPromedio $personas]

# Imprime la edad promedio
puts "La edad promedio de las personas es: $edadPromedio"
```

Este código realiza lo siguiente:

1. Define una estructura de datos para representar una persona utilizando el procedimiento `Persona`, que recibe el nombre, el apellido y la edad de la persona y devuelve una lista con esta información.
2. Crea una lista de personas utilizando la variable `personas`.
3. Define un procedimiento para imprimir la información de una persona utilizando el procedimiento `ImprimirPersona`, que recibe una persona y la imprime en la salida estándar.
4. Recorre la lista de personas e imprime la información de cada una utilizando el procedimiento `foreach`.
5. Define un procedimiento para calcular la edad promedio de las personas utilizando el procedimiento `EdadPromedio`, que recibe una lista de personas y devuelve la edad promedio de las mismas.
6. Calcula la edad promedio de las personas y la asigna a la variable `edadPromedio`.
7. Imprime la edad promedio en la salida estándar.