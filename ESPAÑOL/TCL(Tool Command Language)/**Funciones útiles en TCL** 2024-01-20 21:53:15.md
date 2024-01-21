```tcl
# Crear una función que reciba una cadena y devuelva la misma cadena con todas las letras mayúsculas:

proc mayusculas {cadena} {
    # Convertir la cadena a mayúsculas utilizando la función "string toUpper".
    set cadena_mayusculas [string toUpper $cadena]

    # Devolver la cadena convertida a mayúsculas.
    return $cadena_mayusculas
}

# Ejemplo de uso:

set cadena_original "Hola mundo"
set cadena_mayusculas [mayusculas $cadena_original]

puts "Cadena original: $cadena_original"
puts "Cadena en mayúsculas: $cadena_mayusculas"

# Crear una función que reciba un número y devuelva el factorial de ese número:

proc factorial {numero} {
    # Si el número es menor o igual que 1, devolver 1.
    if {$numero <= 1} {
        return 1
    }

    # De lo contrario, calcular el factorial recursivamente.
    set factorial [expr {$numero * [factorial [expr {$numero - 1}]]}]

    # Devolver el factorial calculado.
    return $factorial
}

# Ejemplo de uso:

set numero = 5
set factorial_numero [factorial $numero]

puts "Factorial de $numero: $factorial_numero"

# Crear una función que reciba una lista de números y devuelva la suma de todos los números de la lista:

proc suma_lista {lista} {
    # Si la lista está vacía, devolver 0.
    if {[llength $lista] == 0} {
        return 0
    }

    # Sumar el primer elemento de la lista al resultado de la suma de los elementos restantes de la lista.
    set suma [expr {[lindex $lista 0] + [suma_lista [lrange $lista 1 end]]}]

    # Devolver la suma calculada.
    return $suma
}

# Ejemplo de uso:

set lista = {1 2 3 4 5}
set suma_lista [suma_lista $lista]

puts "Suma de la lista: $suma_lista"
```

Explicación del código:

* La función `mayusculas` recibe una cadena como argumento y la convierte a mayúsculas utilizando la función `string toUpper`.
* La función `factorial` recibe un número como argumento y calcula el factorial de ese número utilizando la recursividad.
* La función `suma_lista` recibe una lista de números como argumento y devuelve la suma de todos los números de la lista utilizando la recursividad.

El código está comentado para facilitar su comprensión.