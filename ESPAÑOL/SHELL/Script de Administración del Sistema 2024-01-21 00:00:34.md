#!/bin/bash

# Definición de variables
directorio_actual=$(pwd)
archivo_log="/tmp/script.log"
hora_inicio=$(date +%s)

# Función para mostrar un mensaje de error y salir
function error() {
  echo "Error: $1" >&2
  exit 1
}

# Función para escribir una línea en el archivo de log
function log() {
  echo "$1" >> "$archivo_log"
}

# Función para verificar si un comando se ejecutó correctamente
function verificar_comando() {
  if [ $? -ne 0 ]; then
    error "El comando '$1' no se ejecutó correctamente"
  fi
}

# Función para recopilar información sobre el sistema
function recopilar_informacion() {
  log "Recopilando información sobre el sistema..."

  # Obtener el nombre del host
  nombre_host=$(hostname)
  log "Nombre del host: $nombre_host"

  # Obtener la versión del sistema operativo
  version_so=$(lsb_release -a | grep 'Descripción:' | awk '{print $2}')
  log "Versión del sistema operativo: $version_so"

  # Obtener la arquitectura del sistema
  arquitectura=$(uname -m)
  log "Arquitectura del sistema: $arquitectura"

  # Obtener la cantidad de memoria RAM disponible en el sistema
  memoria_ram=$(free -m | grep 'Mem:' | awk '{print $2}')
  log "Memoria RAM disponible: $memoria_ram MB"

  # Obtener la cantidad de espacio libre en el disco duro
  espacio_libre=$(df -h | grep '/dev/' | awk '{print $4}')
  log "Espacio libre en el disco duro: $espacio_libre"

  # Obtener la lista de usuarios registrados en el sistema
  usuarios=$(w | awk '{print $1}')
  log "Usuarios registrados en el sistema: $usuarios"

  # Obtener la lista de procesos en ejecución
  procesos=$(ps -A | awk '{print $1}')
  log "Procesos en ejecución: $procesos"

  # Obtener la lista de servicios en ejecución
  servicios=$(systemctl list-units --type=service | awk '{print $1}')
  log "Servicios en ejecución: $servicios"

  log "Información recopilada satisfactoriamente"
}

# Función para realizar copias de seguridad de archivos y directorios
function realizar_copias_de_seguridad() {
  log "Realizando copias de seguridad de archivos y directorios..."

  # Crear el directorio para las copias de seguridad
  directorio_copias_de_seguridad="/tmp/copias_de_seguridad"
  mkdir -p "$directorio_copias_de_seguridad"
  verificar_comando "mkdir -p $directorio_copias_de_seguridad"

  # Copiar los archivos y directorios seleccionados
  archivos_a_copiar="
    /etc/passwd
    /etc/shadow
    /etc/hosts
    /etc/resolv.conf
    /home/*/.*
  "

  for archivo_o_directorio in $archivos_a_copiar; do
    cp -r "$archivo_o_directorio" "$directorio_copias_de_seguridad"
    verificar_comando "cp -r $archivo_o_directorio $directorio_copias_de_seguridad"
  done

  log "Copias de seguridad realizadas satisfactoriamente"
}

# Función para instalar un paquete
function instalar_paquete() {
  log "Instalando el paquete $1..."

  # Verificar si el paquete ya está instalado
  dpkg -s "$1" &> /dev/null
  if [ $? -eq 0 ]; then
    log "El paquete $1 ya está instalado"
    return
  fi

  # Instalar el paquete
  apt-get install -y "$1"
  verificar_comando "apt-get install -y $1"

  log "El paquete $1 se instaló satisfactoriamente"
}

# Función para desinstalar un paquete
function desinstalar_paquete() {
  log "Desinstalando el paquete $1..."

  # Verificar si el paquete está instalado
  dpkg -s "$1" &> /dev/null
  if [ $? -ne 0 ]; then
    log "El paquete $1 no está instalado"
    return
  fi

  # Desinstalar el paquete
  apt-get remove -y "$1"
  verificar_comando "apt-get remove -y $1"

  log "El paquete $1 se desinstaló satisfactoriamente"
}

# Función para actualizar el sistema
function actualizar_sistema() {
  log "Actualizando el sistema..."

  # Actualizar la lista de paquetes disponibles
  apt-get update
  verificar_comando "apt-get update"

  # Actualizar los paquetes instalados
  apt-get upgrade -y
  verificar_comando "apt-get upgrade -y"

  log "El sistema se actualizó satisfactoriamente"
}

# Función para limpiar el sistema
function limpiar_sistema() {
  log "limpiando el sistema..."

  # Eliminar paquetes innecesarios
  apt-get autoremove -y
  verificar_comando "apt-get autoremove -y"

  # Eliminar archivos temporales
  rm -rf /tmp/*
  verificar_comando "rm -rf /tmp/*"

  log "El sistema se limpió satisfactoriamente"
}

# Función principal
function main() {
  # Cambiar al directorio actual
  cd "$directorio_actual"

  # Registrar la hora de inicio del script
  hora_inicio=$(date +%s)

  # Mostrar un mensaje de bienvenida
  echo "Bienvenido al script de administración del sistema"

  # Verificar si el usuario tiene privilegios de root
  if [ $UID -ne 0 ]; then
    error "Este script debe ejecutarse con privilegios de root"
  fi

  # Mostrar el menú principal
  while true; do
    clear
    echo "Menú principal:"
    echo
    echo "1. Recopilar información sobre el sistema"
    echo "2. Realizar copias de seguridad de archivos y directorios"
    echo "3. Instalar un paquete"
    echo "4. Desinstalar un paquete"
    echo "5. Actualizar el sistema"
    echo "6. Limpiar el sistema"
    echo "7. Salir"
    echo
    echo -n "Seleccione una opción: "

    # Leer la opción seleccionada por el usuario
    read opcion

    # Procesar la opción seleccionada
    case $opcion in
      1)
        recopilar_informacion
        ;;
      2)
        realizar_copias_de_seguridad
        ;;
      3)
        echo -n "Escriba el nombre del paquete que desea instalar: "
        read paquete
        instalar_paquete "$paquete"
        ;;
      4)
        echo -n "Escriba el nombre del paquete que desea desinstalar: "
        read paquete
        desinstalar_paquete "$paquete"
        ;;
      5)
        actualizar_sistema
        ;;
      6)
        limpiar_sistema
        ;;
      7)
        echo "Gracias por utilizar el script de administración del sistema"
        exit 0
        ;;
      *)
        echo "Opción no válida"
        ;;
    esac
  done
}

# Ejecutar la función principal
main