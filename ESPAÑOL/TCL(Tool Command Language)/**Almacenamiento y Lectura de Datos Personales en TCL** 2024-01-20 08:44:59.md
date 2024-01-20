```tcl
# Definición de variables
set nombre "Juan Pérez"
set edad 30
set ciudad "Caracas"
set país "Venezuela"

# Creación de una lista con los datos personales
set datos_personales [list $nombre $edad $ciudad $país]

# Definición de una función para mostrar los datos personales
proc mostrar_datos_personales {datos} {
    foreach dato $datos {
        puts "$dato"
    }
}

# Llamada a la función para mostrar los datos personales
mostrar_datos_personales $datos_personales

# Creación de un diccionario con los datos personales
set datos_personales_diccionario [dict create nombre $nombre edad $edad ciudad $ciudad país $país]

# Definición de una función para mostrar los datos personales en formato de diccionario
proc mostrar_datos_personales_diccionario {diccionario} {
    foreach clave valor [dict keys $diccionario] {
        puts "$clave: $valor"
    }
}

# Llamada a la función para mostrar los datos personales en formato de diccionario
mostrar_datos_personales_diccionario $datos_personales_diccionario

# Creación de una cadena de caracteres con los datos personales en formato JSON
set datos_personales_json [json encode $datos_personales_diccionario]

# Definición de una función para mostrar los datos personales en formato JSON
proc mostrar_datos_personales_json {json} {
    puts "$json"
}

# Llamada a la función para mostrar los datos personales en formato JSON
mostrar_datos_personales_json $datos_personales_json

# Creación de un archivo de texto con los datos personales en formato JSON
set archivo_datos_personales "datos_personales.json"
file write $archivo_datos_personales $datos_personales_json

# Definición de una función para leer los datos personales desde un archivo de texto en formato JSON
proc leer_datos_personales_json {archivo} {
    set datos_personales_json [file read $archivo]
    return [json decode $datos_personales_json]
}

# Llamada a la función para leer los datos personales desde un archivo de texto en formato JSON
set datos_personales_diccionario_leidos [leer_datos_personales_json $archivo_datos_personales]

# Definición de una función para mostrar los datos personales leídos desde un archivo de texto en formato JSON
proc mostrar_datos_personales_leidos {datos} {
    foreach clave valor $datos {
        puts "$clave: $valor"
    }
}

# Llamada a la función para mostrar los datos personales leídos desde un archivo de texto en formato JSON
mostrar_datos_personales_leidos $datos_personales_diccionario_leidos
```

**Explicación del código:**

1. **Definición de Variables:**
   - Se definen las variables necesarias para almacenar los datos personales: nombre, edad, ciudad y país.


2. **Creación de una Lista:**
   - Se crea una lista con los datos personales.


3. **Definición de una Función:**
   - Se define una función para mostrar los datos personales.


4. **Llamada a la Función:**
   - Se llama a la función para mostrar los datos personales.


5. **Creación de un Diccionario:**
   - Se crea un diccionario con los datos personales.


6. **Definición de una Función:**
   - Se define una función para mostrar los datos personales en formato de diccionario.


7. **Llamada a la Función:**
   - Se llama a la función para mostrar los datos personales en formato de diccionario.


8. **Creación de una Cadena JSON:**
   - Se crea una cadena de caracteres con los datos personales en formato JSON.


9. **Definición de una Función:**
   - Se define una función para mostrar los datos personales en formato JSON.


10. **Llamada a la Función:**
    - Se llama a la función para mostrar los datos personales en formato JSON.


11. **Creación de un Archivo:**
    - Se crea un archivo de texto con los datos personales en formato JSON.


12. **Definición de una Función:**
    - Se define una función para leer los datos personales desde un archivo de texto en formato JSON.


13. **Llamada a la Función:**
    - Se llama a la función para leer los datos personales desde un archivo de texto en formato JSON.


14. **Definición de una Función:**
    - Se define una función para mostrar los datos personales leídos desde un archivo de texto en formato JSON.


15. **Llamada a la Función:**
    - Se llama a la función para mostrar los datos personales leídos desde un archivo de texto en formato JSON.