```tcl
# Definir una variable con una lista de nombres
nombres = {"Juan", "María", "Pedro"}

# Definir una función para saludar a una persona
proc saludar {nombre} {
    puts "[Saludar] Hola, $nombre!"
}

# Definir una función para despedirse de una persona
proc despedir {nombre} {
    puts "[Despedir] Adiós, $nombre!"
}

# Iterar sobre la lista de nombres y saludar a cada persona
foreach nombre $nombres {
    saludar $nombre
}

# Iterar sobre la lista de nombres y despedirse de cada persona
foreach nombre $nombres {
    despedir $nombre
}
```

Este código en TCL realiza las siguientes tareas:

1. Define una variable `nombres` con una lista de nombres.
2. Define dos funciones: `saludar` y `despedir`, que se encargan de saludar y despedirse de una persona, respectivamente.
3. Itera sobre la lista de nombres y llama a la función `saludar` para saludar a cada persona.
4. Itera de nuevo sobre la lista de nombres y llama a la función `despedir` para despedirse de cada persona.

El resultado de ejecutar este código será el siguiente:

```
[Saludar] Hola, Juan!
[Saludar] Hola, María!
[Saludar] Hola, Pedro!
[Despedir] Adiós, Juan!
[Despedir] Adiós, María!
[Despedir] Adiós, Pedro!
```