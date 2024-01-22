```shell
#!/bin/bash

# Este script es un ejemplo de un código complejo en SHELL.

# Función para mostrar un mensaje de error.
mostrar_error() {
  echo "Error: $1" >&2
  exit 1
}

# Función para obtener la fecha y hora actual.
obtener_fecha_hora() {
  date +%Y-%m-%d_%H:%M:%S
}

# Función para crear un directorio.
crear_directorio() {
  mkdir -p "$1" || mostrar_error "No se pudo crear el directorio $1."
}

# Función para copiar un archivo.
copiar_archivo() {
  cp "$1" "$2" || mostrar_error "No se pudo copiar el archivo $1 a $2."
}

# Función para mover un archivo.
mover_archivo() {
  mv "$1" "$2" || mostrar_error "No se pudo mover el archivo $1 a $2."
}

# Función para eliminar un archivo.
eliminar_archivo() {
  rm -f "$1" || mostrar_error "No se pudo eliminar el archivo $1."
}

# Función para comprimir un archivo.
comprimir_archivo() {
  gzip -f "$1" || mostrar_error "No se pudo comprimir el archivo $1."
}

# Función para descomprimir un archivo.
descomprimir_archivo() {
  gunzip -f "$1" || mostrar_error "No se pudo descomprimir el archivo $1."
}

# Función para listar los archivos en un directorio.
listar_archivos() {
  ls -l "$1" || mostrar_error "No se pudo listar los archivos en el directorio $1."
}

# Función para buscar un archivo.
buscar_archivo() {
  find "$1" -name "$2" || mostrar_error "No se pudo encontrar el archivo $2 en el directorio $1."
}

# Función para ejecutar un comando.
ejecutar_comando() {
  "$@" || mostrar_error "No se pudo ejecutar el comando: $@"
}

# Función principal del script.
main() {
  # Crear un directorio llamado "copia_de_seguridad".
  crear_directorio copia_de_seguridad

  # Copiar el archivo "archivo.txt" al directorio "copia_de_seguridad".
  copiar_archivo archivo.txt copia_de_seguridad/archivo.txt

  # Mover el archivo "archivo.txt" al directorio "copia_de_seguridad".
  mover_archivo archivo.txt copia_de_seguridad/archivo.txt

  # Eliminar el archivo "archivo.txt".
  eliminar_archivo archivo.txt

  # Comprimir el archivo "copia_de_seguridad/archivo.txt".
  comprimir_archivo copia_de_seguridad/archivo.txt

  # Descomprimir el archivo "copia_de_seguridad/archivo.txt.gz".
  descomprimir_archivo copia_de_seguridad/archivo.txt.gz

  # Listar los archivos en el directorio "copia_de_seguridad".
  listar_archivos copia_de_seguridad

  # Buscar el archivo "archivo.txt" en el directorio "copia_de_seguridad".
  buscar_archivo copia_de_seguridad archivo.txt

  # Ejecutar el comando "ls -l /tmp".
  ejecutar_comando ls -l /tmp
}

# Llamar a la función principal del script.
main
```

Explicación del código:

* La función `mostrar_error()` muestra un mensaje de error y sale del script con un código de error.
* La función `obtener_fecha_hora()` obtiene la fecha y hora actual en formato YYYY-MM-DD_HH:MM:SS.
* La función `crear_directorio()` crea un directorio si no existe.
* La función `copiar_archivo()` copia un archivo a otra ubicación.
* La función `mover_archivo()` mueve un archivo a otra ubicación.
* La función `eliminar_archivo()` elimina un archivo.
* La función `comprimir_archivo()` comprime un archivo en formato gzip.
* La función `descomprimir_archivo()` descomprime un archivo en formato gzip.
* La función `listar_archivos()` lista los archivos en un directorio.
* La función `buscar_archivo()` busca un archivo en un directorio.
* La función `ejecutar_comando()` ejecuta un comando en el shell.
* La función `main()` es la función principal del script. En esta función se llaman a las demás funciones para realizar las tareas necesarias.

El código primero crea un directorio llamado "copia_de_seguridad". Luego, copia el archivo "archivo.txt" al directorio "copia_de_seguridad". A continuación, mueve el archivo "archivo.txt" al directorio "copia_de_seguridad". Después, elimina el archivo "archivo.txt". A continuación, comprime el archivo "copia_de_seguridad/archivo.txt" en formato gzip. Luego, descomprime el archivo "copia_de_seguridad/archivo.txt.gz". A continuación, lista los archivos en el directorio "copia_de_seguridad". Luego, busca el archivo "archivo.txt" en el directorio "copia_de_seguridad". Finalmente, ejecuta el comando "ls -l /tmp".