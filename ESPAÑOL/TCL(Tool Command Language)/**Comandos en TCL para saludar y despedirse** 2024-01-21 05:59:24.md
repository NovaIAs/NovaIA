```tcl
# Definiciones de comandos
proc hola_mundo {mensaje} {
    puts [format "Hola, %s!", $mensaje]
}

proc saludar {nombre} {
    puts [format "Hola, %s! ¿Cómo estás?", $nombre]
}

proc despedirse {nombre} {
    puts [format "Adiós, %s! Que tengas un buen día.", $nombre]
}

# Bloque principal del programa
set mensaje "mundo"
hola_mundo $mensaje

set nombre "Juan"
saludar $nombre

despedirse $nombre
```

Explicación del código:

* **Definiciones de comandos:** Se definen tres comandos: `hola_mundo`, `saludar` y `despedirse`. Cada uno de estos comandos toma un argumento.

* **Bloque principal del programa:** Se establecen los valores de las variables `mensaje` y `nombre`. A continuación, se llaman a los comandos `hola_mundo`, `saludar` y `despedirse`, pasando los valores de las variables como argumentos.

**Salida:**

```
Hola, mundo!
Hola, Juan! ¿Cómo estás?
Adiós, Juan! Que tengas un buen día.
```