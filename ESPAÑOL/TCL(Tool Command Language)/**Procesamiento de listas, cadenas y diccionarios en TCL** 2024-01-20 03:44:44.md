```tcl
# Definir una lista de números del 1 al 10
lista_numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# Crear un procedimiento para calcular la suma de una lista
proc sumar_lista {lista} {
    set suma 0
    foreach número $lista {
        incr suma $número
    }
    return $suma
}

# Crear un procedimiento para calcular el producto de una lista
proc multiplicar_lista {lista} {
    set producto 1
    foreach número $lista {
        set producto [expr $producto * $número]
    }
    return $producto
}

# Crear un procedimiento para mostrar los elementos de una lista
proc mostrar_lista {lista} {
    puts "Elementos de la lista:"
    foreach número $lista {
        puts $número
    }
}

# Calcular la suma y el producto de la lista_numeros
suma_lista_numeros = sumar_lista $lista_numeros
producto_lista_numeros = multiplicar_lista $lista_numeros

# Mostrar los resultados
puts "Suma de la lista de números: $suma_lista_numeros"
puts "Producto de la lista de números: $producto_lista_numeros"

# Crear una lista de cadenas
lista_cadenas = ["Hola", "Mundo", "TCL", "Es", "Genial"]

# Mostrar los elementos de la lista_cadenas
mostrar_lista $lista_cadenas

# Crear un diccionario
diccionario = [
    "nombre" "Juan",
    "apellido" "García",
    "edad" 25
]

# Mostrar el valor de la clave "nombre" del diccionario
puts "Nombre: $diccionario(nombre)"

# Añadir una nueva clave al diccionario
diccionario(ocupación) "Programador"

# Mostrar el diccionario completo
puts "Diccionario:"
foreach clave [dict keys $diccionario] {
    puts "$clave: $diccionario($clave)"
}
```

Explicación del código:

* Definimos una lista de números del 1 al 10 utilizando la lista cuadrada `[]`.
* Creamos tres procedimientos `sumar_lista`, `multiplicar_lista` y `mostrar_lista` para calcular la suma, el producto y mostrar los elementos de una lista, respectivamente.
* Calculamos la suma y el producto de la lista_numeros utilizando los procedimientos creados anteriormente.
* Mostramos los resultados de la suma y el producto.
* Creamos una lista de cadenas utilizando la lista cuadrada `[]`.
* Mostramos los elementos de la lista_cadenas utilizando el procedimiento `mostrar_lista`.
* Creamos un diccionario utilizando la lista cuadrada `[]`.
* Mostramos el valor de la clave "nombre" del diccionario.
* Añadimos una nueva clave "ocupación" al diccionario.
* Mostramos el diccionario completo utilizando un bucle `foreach`.