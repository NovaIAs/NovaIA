```tcl
# Creación de una lista de enteros del 1 al 10
lista_enteros = [range 1 11]

# Definición de una función para calcular el factorial de un número
proc factorial {n} {
    if {[string compare $n 0] == -1} {
        error "El factorial de un número negativo no está definido"
    } elseif {[string compare $n 0] == 0} {
        return 1
    } else {
        return [expr {$n * [factorial [incr $n -1]]}]
    }
}

# Creación de una lista de factoriales de los números de la lista_enteros
lista_factoriales = [lmap factorial $lista_enteros]

# Impresión de la lista de factoriales
puts "Lista de factoriales:"
foreach factorial $lista_factoriales {
    puts "$factorial"
}

# Creación de un diccionario con los números de la lista_enteros como claves y sus respectivos factoriales como valores
diccionario_factoriales = [dict create]
foreach {numero factorial} [lzip $lista_enteros $lista_factoriales] {
    [dict set diccionario_factoriales $numero $factorial]
}

# Impresión del diccionario de factoriales
puts "Diccionario de factoriales:"
foreach {numero factorial} [dict keys $diccionario_factoriales] {
    puts "$numero: $factorial"
}

# Creación de una matriz con los números de la lista_enteros como filas y sus respectivos factoriales como columnas
matriz_factoriales = [lrepeat $] $#lista_enteros [lrepeat $] $#lista_factoriales]
foreach {numero factorial} [lzip $lista_enteros $lista_factoriales] {
    [set matriz_factoriales [expr {$numero - 1}] [expr {$factorial - 1}]] $factorial
}

# Impresión de la matriz de factoriales
puts "Matriz de factoriales:"
for {set i 0} {$i < $} $#matriz_factoriales} {incr i} {
    for {set j 0} {$j < $} $#matriz_factoriales} {incr j} {
        puts -nonewline "$matriz_factoriales($i, $j) "
    }
    puts ""
}
```

Explicación del código:

1. Se crea una lista de enteros del 1 al 10 utilizando el comando `range`.

2. Se define una función llamada `factorial` que calcula el factorial de un número dado.

3. Se crea una lista de factoriales de los números de la `lista_enteros` utilizando el comando `lmap`.

4. Se imprime la lista de factoriales.

5. Se crea un diccionario con los números de la `lista_enteros` como claves y sus respectivos factoriales como valores.

6. Se imprime el diccionario de factoriales.

7. Se crea una matriz con los números de la `lista_enteros` como filas y sus respectivos factoriales como columnas.

8. Se imprime la matriz de factoriales.