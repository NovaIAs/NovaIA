```shell
#!/bin/bash

# Definir funciones

# Función para mostrar un mensaje de error
error() {
  echo "Error: $1" >&2
  exit 1
}

# Función para comprobar si un archivo existe
existe_archivo() {
  [ -f "$1" ]
}

# Función para obtener la fecha actual
fecha_actual() {
  date +%Y-%m-%d
}

# Función para obtener la hora actual
hora_actual() {
  date +%H:%M:%S
}

# Función para crear un directorio
crear_directorio() {
  mkdir -p "$1"
}

# Función para copiar un archivo
copiar_archivo() {
  cp "$1" "$2"
}

# Función para mover un archivo
mover_archivo() {
  mv "$1" "$2"
}

# Función para eliminar un archivo
eliminar_archivo() {
  rm "$1"
}

# Función para comprimir un archivo
comprimir_archivo() {
  tar -czvf "$1.tar.gz" "$1"
}

# Función para descomprimir un archivo
descomprimir_archivo() {
  tar -xzvf "$1.tar.gz"
}

# Función para obtener el tamaño de un archivo
obtener_tamano_archivo() {
  du -sh "$1" | cut -f1
}

# Función para obtener la fecha de modificación de un archivo
obtener_fecha_modificacion_archivo() {
  stat -c %y "$1"
}

# Función para obtener el propietario de un archivo
obtener_propietario_archivo() {
  stat -c %U "$1"
}

# Función para obtener el grupo de un archivo
obtener_grupo_archivo() {
  stat -c %G "$1"
}

# Función para obtener los permisos de un archivo
obtener_permisos_archivo() {
  stat -c %a "$1"
}

# Función para establecer los permisos de un archivo
establecer_permisos_archivo() {
  chmod "$1" "$2"
}

# Función para establecer el propietario de un archivo
establecer_propietario_archivo() {
  chown "$1" "$2"
}

# Función para establecer el grupo de un archivo
establecer_grupo_archivo() {
  chgrp "$1" "$2"
}


# Mostrar un mensaje de bienvenida
echo "Bienvenido al programa de gestión de archivos"

# Obtener la ruta del directorio actual
directorio_actual=$(pwd)

# Mostrar el directorio actual
echo "Directorio actual: $directorio_actual"

# Obtener la lista de archivos del directorio actual
archivos=$(ls -A)

# Mostrar la lista de archivos
echo "Archivos:"
for archivo in $archivos; do
  echo "- $archivo"
done