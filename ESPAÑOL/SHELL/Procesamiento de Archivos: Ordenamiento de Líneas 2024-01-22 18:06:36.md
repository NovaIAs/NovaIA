```
#!/bin/bash

# Declaración de Variables

# Mensajes
MENSAJE_BIENVENIDA="Bienvenido al script de procesamiento de datos"
MENSAJE_SELECCION_ARCHIVO="Por favor, seleccione el archivo que desea procesar:"
MENSAJE_ERROR_ARCHIVO="Lo siento, el archivo seleccionado no existe o no se puede acceder a él."
MENSAJE_PROCESANDO="Procesando el archivo..."
MENSAJE_COMPLETADO="El procesamiento del archivo se ha completado."

# Rutas y Nombres de Archivos
DIRECTORIO_ARCHIVOS="archivos"
ARCHIVO_ENTRADA="${DIRECTORIO_ARCHIVOS}/datos.txt"
ARCHIVO_SALIDA="${DIRECTORIO_ARCHIVOS}/resultados.txt"

# Funciones

# Función para mostrar un mensaje
function mostrar_mensaje() {
  echo "$1"
}

# Función para obtener la selección del usuario
function obtener_seleccion() {
  read -p "$1" seleccion
}

# Función para validar la selección del usuario
function validar_seleccion() {
  if [[ ! -f "$1/${seleccion}" ]]; then
    mostrar_mensaje "$MENSAJE_ERROR_ARCHIVO"
    obtener_seleccion "$MENSAJE_SELECCION_ARCHIVO"
  fi
}

# Función para procesar el archivo
function procesar_archivo() {
  mostrar_mensaje "$MENSAJE_PROCESANDO"
  # Aquí se realiza el procesamiento del archivo.
  # En este ejemplo, simplemente se ordenan las líneas del archivo y se guardan en el archivo de salida.
  sort "$1" > "$2"
  mostrar_mensaje "$MENSAJE_COMPLETADO"
}

# Inicio del Script

# Mostrar el mensaje de bienvenida
mostrar_mensaje "$MENSAJE_BIENVENIDA"

# Obtener la selección del usuario para el archivo de entrada
obtener_seleccion "$MENSAJE_SELECCION_ARCHIVO"

# Validar la selección del usuario
validar_seleccion "$DIRECTORIO_ARCHIVOS"

# Procesar el archivo
procesar_archivo "$ARCHIVO_ENTRADA" "$ARCHIVO_SALIDA"
```

Explicación:

* El script comienza con la definición de variables, incluyendo mensajes, rutas y nombres de archivos, y funciones.
* La función `mostrar_mensaje` simplemente muestra un mensaje en la consola.
* La función `obtener_seleccion` solicita al usuario que ingrese una selección y la almacena en la variable `seleccion`.
* La función `validar_seleccion` verifica si el archivo seleccionado existe y es accesible. Si no es así, muestra un mensaje de error y solicita al usuario que vuelva a ingresar una selección.
* La función `procesar_archivo` procesa el archivo seleccionado. En este ejemplo, simplemente ordena las líneas del archivo y las guarda en un nuevo archivo.
* El script principal comienza mostrando el mensaje de bienvenida.
* Luego, solicita al usuario que ingrese la selección del archivo de entrada y valida la selección.
* Finalmente, procesa el archivo seleccionado y muestra un mensaje de confirmación.