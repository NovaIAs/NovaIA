```shell
#!/bin/bash

# Declaración de variables
directorio_actual=$(pwd)
archivos=$(ls -1 "$directorio_actual")
contador_archivos=0

# Función para contar los archivos en un directorio
contar_archivos() {
  for archivo in "$archivos"; do
    if [ -f "$archivo" ]; then
      ((contador_archivos++))
    fi
  done
}

# Función para mostrar el contenido de un archivo
mostrar_contenido_archivo() {
  if [ -f "$1" ]; then
    cat "$1"
  else
    echo "El archivo $1 no existe."
  fi
}

# Función para crear un nuevo archivo
crear_archivo() {
  if [ -z "$1" ]; then
    echo "Debes especificar un nombre de archivo."
  else
    touch "$1"
    echo "Archivo $1 creado."
  fi
}

# Función para eliminar un archivo
eliminar_archivo() {
  if [ -z "$1" ]; then
    echo "Debes especificar un nombre de archivo."
  else
    rm "$1"
    echo "Archivo $1 eliminado."
  fi
}

# Función para copiar un archivo
copiar_archivo() {
  if [ -z "$1" ] || [ -z "$2" ]; then
    echo "Debes especificar un archivo de origen y un archivo de destino."
  else
    cp "$1" "$2"
    echo "Archivo $1 copiado a $2."
  fi
}

# Función para mover un archivo
mover_archivo() {
  if [ -z "$1" ] || [ -z "$2" ]; then
    echo "Debes especificar un archivo de origen y un archivo de destino."
  else
    mv "$1" "$2"
    echo "Archivo $1 movido a $2."
  fi
}

# Función para renombrar un archivo
renombrar_archivo() {
  if [ -z "$1" ] || [ -z "$2" ]; then
    echo "Debes especificar un nombre de archivo actual y un nuevo nombre de archivo."
  else
    mv "$1" "$2"
    echo "Archivo $1 renombrado a $2."
  fi
}

# Función para crear un directorio
crear_directorio() {
  if [ -z "$1" ]; then
    echo "Debes especificar un nombre de directorio."
  else
    mkdir "$1"
    echo "Directorio $1 creado."
  fi
}

# Función para eliminar un directorio
eliminar_directorio() {
  if [ -z "$1" ]; then
    echo "Debes especificar un nombre de directorio."
  else
    rmdir "$1"
    echo "Directorio $1 eliminado."
  fi
}

# Función para mostrar el contenido de un directorio
mostrar_contenido_directorio() {
  if [ -d "$1" ]; then
    ls -l "$1"
  else
    echo "El directorio $1 no existe."
  fi
}

# Función para copiar un directorio
copiar_directorio() {
  if [ -z "$1" ] || [ -z "$2" ]; then
    echo "Debes especificar un directorio de origen y un directorio de destino."
  else
    cp -r "$1" "$2"
    echo "Directorio $1 copiado a $2."
  fi
}

# Función para mover un directorio
mover_directorio() {
  if [ -