```tcl
# Crear una lista de números del 1 al 10
lista_numeros = [list range 1 11]

# Crear una función para calcular el factorial de un número
proc factorial arg {
    if {$arg == 0} {
        return 1
    }
    return [expr $arg * [factorial [expr $arg - 1]]]
}

# Iterar sobre la lista de números y calcular el factorial de cada uno
for {set num 0} {$num < [llength $lista_numeros]} {incr num} {
    set factorial_num [factorial [lindex $lista_numeros $num]]
    puts "${num}! = ${factorial_num}"
}
```

Explicación:

1. Creamos una lista de números del 1 al 10 utilizando el comando `list`.

2. Creamos una función llamada `factorial` que calcula el factorial de un número utilizando recursión.

3. Iteramos sobre la lista de números utilizando el comando `for`.

4. Para cada número en la lista, calculamos el factorial utilizando la función `factorial`.

5. Imprimimos el resultado del factorial utilizando el comando `puts`.