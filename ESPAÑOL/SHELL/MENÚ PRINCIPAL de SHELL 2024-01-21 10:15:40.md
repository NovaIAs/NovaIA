```shell
#!/bin/bash

# Función para mostrar el menú principal
function mostrar_menu_principal() {
  clear
  echo "Bienvenido al menú principal."
  echo "Seleccione una opción:"
  echo "1) Mostrar información del sistema"
  echo "2) Crear un nuevo archivo"
  echo "3) Editar un archivo existente"
  echo "4) Copiar un archivo"
  echo "5) Mover un archivo"
  echo "6) Eliminar un archivo"
  echo "7) Salir"
}

# Función para mostrar información del sistema
function mostrar_informacion_sistema() {
  clear
  echo "Información del sistema:"
  echo "Nombre de la máquina: $(hostname)"
  echo "Sistema operativo: $(uname -s)"
  echo "Versión del sistema operativo: $(uname -r)"
  echo "Arquitectura del sistema operativo: $(uname -m)"
  echo "Procesador: $(uname -p)"
  echo "Memoria RAM: $(free -m | grep Mem | awk '{print $2}') MB"
  echo "Espacio en disco duro: $(df -h | grep /dev/ | awk '{print $2}')"
  echo "Tiempo de actividad: $(uptime | awk '{print $3}')"
}

# Función para crear un nuevo archivo
function crear_archivo() {
  clear
  echo "Crear un nuevo archivo:"
  echo "Introduzca el nombre del archivo:"
  read nombre_archivo
  echo "Introduzca el contenido del archivo:"
  read contenido_archivo
  echo "$contenido_archivo" > "$nombre_archivo"
  echo "Archivo creado correctamente."
}

# Función para editar un archivo existente
function editar_archivo() {
  clear
  echo "Editar un archivo existente:"
  echo "Introduzca el nombre del archivo:"
  read nombre_archivo
  if [ -f "$nombre_archivo" ]; then
    vim "$nombre_archivo"
    echo "Archivo editado correctamente."
  else
    echo "El archivo no existe."
  fi
}

# Función para copiar un archivo
function copiar_archivo() {
  clear
  echo "Copiar un archivo:"
  echo "Introduzca el nombre del archivo a copiar:"
  read nombre_archivo_origen
  echo "Introduzca el nombre del archivo de destino:"
  read nombre_archivo_destino
  if [ -f "$nombre_archivo_origen" ]; then
    cp "$nombre_archivo_origen" "$nombre_archivo_destino"
    echo "Archivo copiado correctamente."
  else
    echo "El archivo a copiar no existe."
  fi
}

# Función para mover un archivo
function mover_archivo() {
  clear
  echo "Mover un archivo:"
  echo "Introduzca el nombre del archivo a mover:"
  read nombre_archivo_origen
  echo "Introduzca el nombre del directorio de destino:"
  read directorio_destino
  if [ -f "$nombre_archivo_origen" ]; then
    mv "$nombre_archivo_origen" "$directorio_destino"
    echo "A