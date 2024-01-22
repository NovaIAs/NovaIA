```tcl
# Crear una lista de números del 1 al 10
lista_numeros [lsort [range 1 11]]

# Imprimir la lista de números
puts "Lista de números:"
foreach num $lista_numeros {
    puts $num
}

# Crear una lista de cadenas de caracteres
lista_cadenas [list "Hola" "Mundo" "!"]

# Imprimir la lista de cadenas de caracteres
puts "Lista de cadenas:"
foreach cadena $lista_cadenas {
    puts $cadena
}

# Crear un diccionario con claves y valores
diccionario [dict create]

# Añadir elementos al diccionario
dict set diccionario "nombre" "Juan"
dict set diccionario "apellido" "Pérez"
dict set diccionario "edad" 30

# Imprimir los elementos del diccionario
puts "Diccionario:"
foreach clave [dict keys $diccionario] {
    puts "$clave: $diccionario($clave)"
}

# Crear un procedimiento para calcular el factorial de un número
proc factorial {num} {
    if {$num == 0} {
        return 1
    }
    return [expr {$num * [factorial [expr {$num - 1}]]}]
}

# Llamar al procedimiento para calcular el factorial de 5
resultado [factorial 5]

# Imprimir el resultado
puts "Factorial de 5: $resultado"

# Crear un objeto con atributos y métodos
class Objeto {
    method constructor {nombre} {
        self set nombre $nombre
    }
    method greet {nombre} {
        puts "Hola, [set nombre [string tolower $nombre]]!"
    }
}

# Crear una instancia del objeto
objeto [Objeto new "Juan"]

# Llamar al método greet del objeto
[objeto greet "JUAN"]

# Crear un script que se ejecuta al arrancar el programa
auto_exec {
    puts "Este script se ejecuta al arrancar el programa"
}

# Comenzar el programa
tclsh
```

Este código cubre una amplia gama de características de TCL, incluyendo listas, cadenas de caracteres, diccionarios, procedimientos, objetos y scripts de inicio automático. También incluye ejemplos de uso de estos elementos en un programa TCL.