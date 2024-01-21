```tcl
# Definimos una función para saludar a una persona.
proc saludar {nombre} {
    puts "Hola, $nombre!"
}

# Definimos una función para despedirse de una persona.
proc despedir {nombre} {
    puts "Adiós, $nombre!"
}

# Definimos una función para crear una lista de nombres.
proc crear_lista_nombres {nombres} {
    return [lreverse $nombres]
}

# Definimos una función para imprimir una lista de nombres.
proc imprimir_lista_nombres {nombres} {
    foreach nombre $nombres {
        puts $nombre
    }
}

# Definimos una función para buscar el índice de un elemento en una lista.
proc buscar_indice {elemento lista} {
    set indice -1
    foreach valor [lreverse $lista] {
        incr indice
        if {$valor == $elemento} {
            return $indice
        }
    }
    return -1
}

# Definimos una función para eliminar un elemento de una lista.
proc eliminar_elemento {elemento lista} {
    set indice [buscar_indice $elemento $lista]
    if {$indice != -1} {
        set nueva_lista [lreplace $lista $indice $indice]
        return $nueva_lista
    }
    return $lista
}

# Definimos una función para añadir un elemento a una lista.
proc añadir_elemento {elemento lista} {
    return [lreverse [linsert $lista end $elemento]]
}

# Creamos una lista de nombres.
set nombres [crear_lista_nombres {Juan María Pedro Ana Sofía}]

# Imprimimos la lista de nombres.
puts "Lista de nombres:"
imprimir_lista_nombres $nombres

# Buscamos el índice del nombre "María" en la lista.
set indice [buscar_indice María $nombres]

# Eliminamos el nombre "María" de la lista.
set nueva_lista [eliminar_elemento María $nombres]

# Añadimos el nombre "Carlos" a la lista.
set nueva_lista [añadir_elemento Carlos $nueva_lista]

# Imprimimos la nueva lista de nombres.
puts "Nueva lista de nombres:"
imprimir_lista_nombres $nueva_lista
```

Explicación del código:

1. Definimos cuatro funciones: `saludar`, `despedir`, `crear_lista_nombres` e `imprimir_lista_nombres`.
2. La función `saludar` toma un nombre como argumento y saluda a la persona con ese nombre.
3. La función `despedir` toma un nombre como argumento y se despide de la persona con ese nombre.
4. La función `crear_lista_nombres` toma una lista de nombres como argumento y crea una nueva lista con los nombres invertidos.
5. La función `imprimir_lista_nombres` toma una lista de nombres como argumento e imprime cada nombre en una línea separada.
6. Definimos una función adicional llamada `buscar_indice` que toma un elemento y una lista como argumentos y devuelve el índice del elemento en la lista.
7. Definimos otra función llamada `eliminar_elemento` que toma un elemento y una lista como argumentos y devuelve una nueva lista con el elemento eliminado.
8. Definimos una tercera función llamada `añadir_elemento` que toma un elemento y una lista como argumentos y devuelve una nueva lista con el elemento añadido al final.
9. Creamos una lista de nombres usando la función `crear_lista_nombres`.
10. Imprimimos la lista de nombres usando la función `imprimir_lista_nombres`.
11. Buscamos el índice del nombre "María" en la lista usando la función `buscar_indice`.
12. Eliminamos el nombre "María" de la lista usando la función `eliminar_elemento`.
13. Añadimos el nombre "Carlos" a la lista usando la función `añadir_elemento`.
14. Imprimimos la nueva lista de nombres usando la función `imprimir_lista_nombres`.