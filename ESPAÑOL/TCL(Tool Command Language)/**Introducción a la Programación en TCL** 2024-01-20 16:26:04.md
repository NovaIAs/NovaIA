```tcl
# Crear variables
set nombre "Juan"
set edad 25
set ciudad "Madrid"

# Crear listas
set frutas {manzana naranja pera}
set numeros {1 2 3 4 5}

# Crear diccionarios
set persona {
    nombre "María"
    edad 30
    ciudad "Barcelona"
}

set cursos {
    python {
        nivel "básico"
        duracion 20
    }
    java {
        nivel "avanzado"
        duracion 30
    }
}

# Usar ciclos para iterar sobre listas y diccionarios
foreach fruta $frutas {
    puts $fruta
}

foreach curso $cursos {
    puts [format "%s: nivel %s, duración %d horas" \
        [dict get $curso nombre] \
        [dict get $curso nivel] \
        [dict get $curso duracion]]
}

# Usar comandos para manipular cadenas
set texto "Hola, mi nombre es $nombre"
puts [string length $texto]

# Usar comandos para interactuar con el sistema
exec echo "Hola $nombre"

# Usar comandos para crear y manipular archivos
file delete tmp.txt
file write tmp.txt "Hola $nombre"
file read tmp.txt
```

**Explicación del código:**

* **Variables:** Se crean variables para almacenar información básica como el nombre, la edad y la ciudad de una persona.
* **Listas:** Se crean listas para almacenar conjuntos de datos homogéneos, como una lista de frutas o una lista de números.
* **Diccionarios:** Se crean diccionarios para almacenar conjuntos de datos heterogéneos, como la información de una persona o la información de un curso.
* **Ciclos:** Se utilizan ciclos para iterar sobre listas y diccionarios, permitiendo recorrer todos los elementos de la lista o diccionario y realizar alguna acción con cada uno de ellos.
* **Comandos para manipular cadenas:** Se utilizan comandos para manipular cadenas de texto, como obtener la longitud de una cadena o reemplazar una subcadena por otra.
* **Comandos para interactuar con el sistema:** Se utilizan comandos para interactuar con el sistema operativo, como ejecutar un comando o crear un archivo.
* **Comandos para crear y manipular archivos:** Se utilizan comandos para crear y manipular archivos, como crear un archivo, escribir en un archivo y leer de un archivo.