```shell
#!/bin/bash

# Variables globales
ruta_script=$(dirname "$0")
ruta_datos="$ruta_script/datos"
ruta_logs="$ruta_script/logs"
nombre_archivo_log="$ruta_logs/script.log"

# Función para registrar mensajes en el log
function log() {
  echo "$(date '+%Y-%m-%d %H:%M:%S') $1" >> "$nombre_archivo_log"
}

# Función para comprobar si un archivo existe
function existe_archivo() {
  if [ -f "$1" ]; then
    return 0
  else
    return 1
  fi
}

# Función para leer una variable de entorno
function leer_variable_entorno() {
  if [ -z "$1" ]; then
    log "Error: La variable de entorno $1 no está definida."
    exit 1
  fi

  export "$1"=$(printenv "$1")
}

# Función para comprobar si una variable de entorno está definida
function esta_definida_variable_entorno() {
  if [ -z "$(printenv "$1")" ]; then
    return 1
  else
    return 0
  fi
}

# Función para crear un directorio si no existe
function crear_directorio() {
  if [ ! -d "$1" ]; then
    mkdir -p "$1"
  fi
}

# Función para copiar un archivo a otro directorio
function copiar_archivo() {
  if existe_archivo "$1"; then
    cp "$1" "$2"
  else
    log "Error: El archivo $1 no existe."
    exit 1
  fi
}

# Función para mover un archivo a otro directorio
function mover_archivo() {
  if existe_archivo "$1"; then
    mv "$1" "$2"
  else
    log "Error: El archivo $1 no existe."
    exit 1
  fi
}

# Función para eliminar un archivo
function eliminar_archivo() {
  if existe_archivo "$1"; then
    rm "$1"
  else
    log "Error: El archivo $1 no existe."
    exit 1
  fi
}

# Función para listar los archivos de un directorio
function listar_archivos() {
  if [ -d "$1" ]; then
    ls -1 "$1"
  else
    log "Error: El directorio $1 no existe."
    exit 1
  fi
}

# Función principal del script
function main() {
  # Leer las variables de entorno necesarias
  leer_variable_entorno API_KEY
  leer_variable_entorno API_URL

  # Comprobar si existe el directorio de datos
  if ! existe_archivo "$ruta_datos"; then
    crear_directorio "$ruta_datos"
  fi

  # Comprobar si existe el directorio de logs
  if ! existe_archivo "$ruta_logs"; then
    crear_directorio "$ruta_logs"
  fi

  # Descargar los datos de la API
  curl -X GET "$API_URL" -H "Authorization: Bearer $API_KEY" -o "$ruta_datos/datos.json"

  # Procesar los datos descargados
  python3 "$ruta_script/procesar_datos.py" "$ruta_datos/datos.json" "$ruta_datos/datos_procesados.csv"

  # Generar un informe con los datos procesados
  Rscript "$ruta_script/generar_informe.r" "$ruta_datos/datos_procesados.csv" "$ruta_datos/informe.pdf"

  # Enviar el informe por correo electrónico
  sendEmail -f "script@ejemplo.com" -t "destinatario@ejemplo.com" -u "Informe diario" -o tls=yes -s smtp.ejemplo.com:587 -xu "usuario@ejemplo.com" -xp "contraseña" -a "$ruta_datos/informe.pdf"

  # Limpiar los archivos temporales
  eliminar_archivo "$ruta_datos/datos.json"
  eliminar_archivo "$ruta_datos/datos_procesados.csv"
}

# Llamar a la función principal del script
main
```

Explicación:

Este script está diseñado para procesar y analizar datos descargados de una API externa.

En primer lugar, el script comprueba si existen los directorios necesarios para almacenar los datos y los logs. Si no existen, los crea.

A continuación, descarga los datos de la API en formato JSON y los almacena en el directorio de datos.

Luego, procesa los datos descargados utilizando un script de Python y los guarda en un archivo CSV.

Posteriormente, genera un informe con los datos procesados utilizando un script de R y lo guarda en formato PDF.

Finalmente, envía el informe por correo electrónico utilizando un programa externo.

El script también elimina los archivos temporales que se crean durante el proceso.