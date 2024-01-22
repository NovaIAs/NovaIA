```tcl
# Definir una lista de ciudades y sus respectivas temperaturas
ciudades = {"Madrid": 20, "Barcelona": 22, "Valencia": 24, "Sevilla": 26, "Málaga": 28}

# Definir un procedimiento para mostrar la temperatura de una ciudad
proc mostrar_temperatura {ciudad} {
    puts "La temperatura en $ciudad es de $ciudades($ciudad) grados Celsius."
}

# Definir un procedimiento para calcular la temperatura promedio de todas las ciudades
proc calcular_promedio {} {
    # Obtener la suma de todas las temperaturas
    set suma 0
    foreach ciudad [array names ciudades] {
        set suma [expr {$suma + $ciudades($ciudad)}]
    }

    # Calcular el promedio
    set promedio [expr {$suma / [llength $ciudades]}]

    # Devolver el promedio
    return $promedio
}

# Definir un procedimiento para mostrar la lista de ciudades y sus respectivas temperaturas
proc mostrar_lista {} {
    puts "Lista de ciudades y sus respectivas temperaturas:"
    foreach ciudad [array names ciudades] {
        puts "$ciudad: $ciudades($ciudad) grados Celsius"
    }
}

# Definir un procedimiento para mostrar la temperatura máxima y mínima
proc mostrar_extremos {} {
    # Obtener la temperatura máxima y mínima
    set maximo [lrange $ciudades end end]
    set minimo [lrange $ciudades 0 0]

    # Mostrar la temperatura máxima y mínima
    puts "La temperatura máxima es: $maximo grados Celsius."
    puts "La temperatura mínima es: $minimo grados Celsius."
}

# Mostrar la temperatura de una ciudad
mostrar_temperatura Madrid

# Calcular y mostrar la temperatura promedio de todas las ciudades
set promedio [calcular_promedio]
puts "La temperatura promedio de todas las ciudades es de $promedio grados Celsius."

# Mostrar la lista de ciudades y sus respectivas temperaturas
mostrar_lista

# Mostrar la temperatura máxima y mínima
mostrar_extremos
```

Este código es más complejo que el anterior y realiza varias operaciones diferentes. En primer lugar, define una lista de ciudades y sus respectivas temperaturas. A continuación, define varios procedimientos que se utilizan para mostrar la temperatura de una ciudad, calcular la temperatura promedio de todas las ciudades, mostrar la lista de ciudades y sus respectivas temperaturas, y mostrar la temperatura máxima y mínima. Por último, llama a los procedimientos para mostrar la información de las ciudades.