```tcl
# Definición de una función para saludar
proc saludar {nombre} {
    puts "Hola, $nombre! ¿Cómo estás hoy?"
}

# Definición de una función para despedirse
proc despedirse {nombre} {
    puts "Adiós, $nombre! Espero verte pronto."
}

# Definición de una función principal
proc main {} {
    # Obtener el nombre del usuario
    puts "Introduce tu nombre:"
    set nombre [gets stdin]

    # Saludar al usuario
    saludar $nombre

    # Preguntar al usuario cómo está
    puts "¿Cómo estás hoy, $nombre?"
    set respuesta [gets stdin]

    # Responder al usuario en función de su respuesta
    if {$respuesta eq "bien"} {
        puts "Me alegro de oírlo."
    } elseif {$respuesta eq "mal"} {
        puts "Lo siento, espero que te sientas mejor pronto."
    } else {
        puts "No entiendo tu respuesta."
    }

    # Despedirse del usuario
    despedirse $nombre
}

# Llamar a la función principal
main
```

Explicación del código:

1. **Definición de funciones**: Se definen dos funciones, `saludar` y `despedirse`, que se utilizan para saludar y despedirse del usuario, respectivamente.

2. **Función `main`**: Se define una función principal llamada `main`, que es el punto de entrada del programa.

3. **Obtener el nombre del usuario**: Se utiliza el comando `puts` para mostrar un mensaje en la consola preguntando al usuario por su nombre, y se utiliza el comando `gets` para obtener la entrada del usuario.

4. **Saludar al usuario**: Se llama a la función `saludar` para saludar al usuario utilizando su nombre.

5. **Preguntar al usuario cómo está**: Se utiliza el comando `puts` para mostrar un mensaje en la consola preguntando al usuario cómo está, y se utiliza el comando `gets` para obtener la entrada del usuario.

6. **Responder al usuario en función de su respuesta**: Se comprueba la respuesta del usuario utilizando una instrucción `if` para determinar si el usuario ha respondido "bien", "mal" o cualquier otra cosa. En función de la respuesta del usuario, se muestra un mensaje adecuado utilizando el comando `puts`.

7. **Despedirse del usuario**: Se llama a la función `despedirse` para despedirse del usuario utilizando su nombre.

8. **Llamar a la función principal**: Se llama a la función `main` para iniciar el programa.