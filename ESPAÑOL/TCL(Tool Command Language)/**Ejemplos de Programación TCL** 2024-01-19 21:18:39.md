```tcl
# Definición de una función para saludar
proc saludar {nombre} {
  puts "[format "Hola, %s!" $nombre]"
}

# Llamada a la función saludar con un valor específico
saludar "Juan"

# Definición de una lista de nombres
nombres = {Juan María Ana Pedro}

# Iteración sobre la lista de nombres utilizando la instrucción foreach
foreach nombre $nombres {
  saludar $nombre
}

# Creación de un diccionario para almacenar información de contacto
contactos = {
  "Juan Pérez" {
    teléfono "555-123-4567"
    correo "juan.perez@ejemplo.com"
  }
  "María García" {
    teléfono "555-234-5678"
    correo "maria.garcia@ejemplo.com"
  }
}

# Recuperación de la información de contacto de María García
set telefono_maria $contactos(María García)(teléfono)
set correo_maria $contactos(María García)(correo)

# Impresión de la información de contacto de María García
puts "[format "El teléfono de María García es %s y su correo electrónico es %s" $telefono_maria $correo_maria]"

# Definición de una expresión regular para validar direcciones de correo electrónico
set regex_correo_electronico "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"

# Validación de una dirección de correo electrónico usando la expresión regular
set direccion_correo_electronico "juan.perez@ejemplo.com"
if {[regexp $regex_correo_electronico $direccion_correo_electronico]} {
  puts "[format "%s es una dirección de correo electrónico válida" $direccion_correo_electronico]"
} else {
  puts "[format "%s no es una dirección de correo electrónico válida" $direccion_correo_electronico]"
}

# Creación de una interfaz de usuario utilizando la extensión Tk
package require Tk
set ventana [toplevel]
set etiqueta [label $ventana -text "Hola, Mundo!"]
pack $etiqueta
mainloop
```

Explicación del código:

1. Se define una función llamada `saludar` que recibe un argumento `nombre` y simplemente imprime un saludo personalizado.
2. Se llama a la función `saludar` con el valor "Juan" como argumento, lo que imprimirá "Hola, Juan!".
3. Se crea una lista de nombres llamada `nombres` y se utiliza la instrucción `foreach` para iterar sobre cada nombre de la lista, llamando a la función `saludar` para cada nombre.
4. Se crea un diccionario llamado `contactos` que mapea nombres a información de contacto. Cada entrada del diccionario es una lista de pares clave-valor, donde la clave es el nombre del contacto y el valor es un diccionario con información de contacto.
5. Se recuperan el teléfono y el correo electrónico de María García del diccionario `contactos` y se imprimen en la pantalla.
6. Se define una expresión regular para validar direcciones de correo electrónico y se utiliza para validar una dirección de correo electrónico específica.
7. Se crea una interfaz de usuario simple utilizando la extensión `Tk`, que incluye una ventana, una etiqueta y un botón.