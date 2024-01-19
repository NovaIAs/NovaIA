```tcl
# Definir una función para saludar a alguien
proc saludar {nombre} {
    # Comprobar si se ha proporcionado un nombre
    if {[llength $nombre] == 0} {
        # Si no se ha proporcionado un nombre, saludar al usuario con un mensaje genérico
        puts "Hola, desconocido!"
    } else {
        # Si se ha proporcionado un nombre, saludar al usuario con su nombre
        puts "Hola, [lindex $nombre 0]!"
    }
}

# Definir una función para generar una lista de números aleatorios
proc generar_lista_aleatorios {longitud} {
    # Comprobar si se ha proporcionado una longitud
    if {[llength $longitud] == 0} {
        # Si no se ha proporcionado una longitud, generar una lista de 10 números aleatorios
        set longitud 10
    }

    # Crear una lista vacía para almacenar los números aleatorios
    set lista {}

    # Generar números aleatorios y añadirlos a la lista
    for {set i 0} {$i < $longitud} {incr i} {
        # Generar un número aleatorio entre 1 y 100
        set numero [expr {int(rand() * 100) + 1}]

        # Añadir el número a la lista
        lappend lista $numero
    }

    # Devolver la lista de números aleatorios
    return $lista
}

# Definir una función para imprimir una lista de elementos
proc imprimir_lista {lista} {
    # Comprobar si se ha proporcionado una lista
    if {[llength $lista] == 0} {
        # Si no se ha proporcionado una lista, imprimir un mensaje de error
        puts "No se ha proporcionado una lista para imprimir."
    } else {
        # Imprimir los elementos de la lista, separados por comas
        puts [join $lista ","]
    }
}

# Saludar al usuario
saludar "Juan"

# Generar una lista de 20 números aleatorios
set lista_aleatorios [generar_lista_aleatorios 20]

# Imprimir la lista de números aleatorios
imprimir_lista $lista_aleatorios
```

Explicación del código:

* La función `saludar` recibe un nombre como argumento y saluda al usuario con su nombre. Si no se proporciona un nombre, saluda al usuario con un mensaje genérico.
* La función `generar_lista_aleatorios` recibe una longitud como argumento y genera una lista de números aleatorios de esa longitud. Si no se proporciona una longitud, se genera una lista de 10 números aleatorios.
* La función `imprimir_lista` recibe una lista como argumento e imprime los elementos de la lista, separados por comas. Si no se proporciona una lista, se imprime un mensaje de error.
* El código principal llama a la función `saludar` para saludar al usuario con su nombre. Luego, llama a la función `generar_lista_aleatorios` para generar una lista de 20 números aleatorios. Por último, llama a la función `imprimir_lista` para imprimir la lista de números aleatorios.