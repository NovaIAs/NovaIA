```tcl
## Ejemplo de código complejo en TCL (Tool Command Language)

## Definición de variables
set nombre "Juan"
set apellido "García"
set edad 25
set pais "España"

## Creación de un procedimiento personalizado
proc saludar {nombre} {
    puts "Hola, $nombre! ¿Cómo estás?"
}

## Uso del procedimiento personalizado
saludar $nombre

## Creación de una lista de elementos
set frutas {manzana naranja pera plátano}

## Iteración sobre una lista usando un bucle `foreach`
foreach fruta $frutas {
    puts "Me gustan las $fruta."
}

## Creación de un diccionario (hash)
set datos {
    nombre $nombre
    apellido $apellido
    edad $edad
    pais $pais
}

## Acceso a los valores de un diccionario
puts [dict get $datos nombre]
puts [dict get $datos apellido]
puts [dict get $datos edad]
puts [dict get $datos pais]

## Manipulación de cadenas de texto
set cadena "Hola, mundo!"

## Obtener la longitud de una cadena
puts [string length $cadena]

## Concatenación de cadenas
set cadena_concatenada [concat $cadena " Soy TCL."]

## Conversión de mayúsculas a minúsculas y viceversa
puts [string toupper $cadena]
puts [string tolower $cadena]

## Búsqueda y reemplazo de texto
set cadena_reemplazada [string replace $cadena "mundo" "universo"]

## Expresiones regulares
set patron "^Hola.*$"
if {[regexp $patron $cadena]} {
    puts "La cadena $cadena coincide con el patrón $patron."
} else {
    puts "La cadena $cadena no coincide con el patrón $patron."
}

## Manejo de errores
catch {
    # Código que puede generar un error
    throw "Error generado intencionalmente."
} error {
    puts "Se ha producido un error: $error"
}

## Creación de una interfaz gráfica de usuario (GUI)
package require Tk

## Crear una ventana principal
set ventana [new Toplevel]

## Establecer el título de la ventana
wm title $ventana "Ejemplo de GUI en TCL"

## Crear un botón
set boton [button $ventana -text "Presioname"]

## Asignar una acción al botón
$boton configure -command {
    puts "Has presionado el botón."
}

## Mostrar la ventana principal
wm deiconify $ventana

## Bucle principal de la aplicación
while {[wm exists $ventana]} {
    update
}
```

Explicación del código:

1. **Definición de variables:** Se definen varias variables con valores específicos, como el nombre, apellido, edad y país de una persona.

2. **Creación de un procedimiento personalizado:** Se define un procedimiento llamado `saludar` que toma un nombre como argumento y muestra un mensaje de saludo.

3. **Uso del procedimiento personalizado:** Se utiliza el procedimiento `saludar` para mostrar un mensaje de saludo al usuario.

4. **Creación de una lista de elementos:** Se crea una lista de frutas llamada `frutas`.

5. **Iteración sobre una lista usando un bucle `foreach`:** Se utiliza un bucle `foreach` para iterar sobre la lista de frutas y mostrar un mensaje para cada una de ellas.

6. **Creación de un diccionario (hash):** Se crea un diccionario llamado `datos` que contiene los datos de una persona, como su nombre, apellido, edad y país.

7. **Acceso a los valores de un diccionario:** Se accede a los valores del diccionario `datos` utilizando la función `dict get`.

8. **Manipulación de cadenas de texto:** Se muestran ejemplos de manipulación de cadenas de texto, como obtener la longitud de una cadena, concatenar cadenas, convertir entre mayúsculas y minúsculas, y buscar y reemplazar texto utilizando expresiones regulares.

9. **Manejo de errores:** Se muestra un ejemplo de manejo de errores utilizando el comando `catch` para capturar errores y mostrar un mensaje de error personalizado.

10. **Creación de una interfaz gráfica de usuario (GUI):** Se crea una sencilla interfaz gráfica de usuario (GUI) utilizando la biblioteca Tk de TCL. Se crea una ventana principal, se añade un botón a la ventana y se asigna una acción al botón.

11. **Bucle principal de la aplicación:** Se utiliza un bucle `while` para mantener la ventana principal abierta y procesar los eventos de la interfaz gráfica de usuario.