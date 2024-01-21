```shell
#!/bin/bash

# Variables globales
DIRECTORIO_BASE="/home/usuario/directorio_base"
ARCHIVO_CONFIGURACION="${DIRECTORIO_BASE}/configuracion.txt"
ARCHIVO_BITACORA="${DIRECTORIO_BASE}/bitacora.log"

# Función para leer el archivo de configuración
function leer_configuracion() {
  # Comprueba si el archivo de configuración existe
  if [ ! -f "${ARCHIVO_CONFIGURACION}" ]; then
    echo "El archivo de configuración no existe"
    exit 1
  fi

  # Lee la primera línea del archivo de configuración y la asigna a la variable $VARIABLE_A
  VARIABLE_A=$(head -n 1 "${ARCHIVO_CONFIGURACION}")

  # Lee la segunda línea del archivo de configuración y la asigna a la variable $VARIABLE_B
  VARIABLE_B=$(tail -n 1 "${ARCHIVO_CONFIGURACION}")
}

# Función para escribir en el archivo de bitácora
function escribir_bitacora() {
  # Comprueba si el directorio base existe
  if [ ! -d "${DIRECTORIO_BASE}" ]; then
    # Crea el directorio base si no existe
    mkdir "${DIRECTORIO_BASE}"
  fi

  # Comprueba si el archivo de bitácora existe
  if [ ! -f "${ARCHIVO_BITACORA}" ]; then
    # Crea el archivo de bitácora si no existe
    touch "${ARCHIVO_BITACORA}"
  fi

  # Escribe el mensaje en el archivo de bitácora
  echo "$(date "+%Y-%m-%d %H:%M:%S") - $1" >> "${ARCHIVO_BITACORA}"
}

# Función principal
function main() {
  # Lee el archivo de configuración
  leer_configuracion

  # Escribe un mensaje en el archivo de bitácora
  escribir_bitacora "Se leyó el archivo de configuración"

  # Ejecuta un comando
  comando_a_ejecutar="comando_a_ejecutar"
  salida_comando=$(eval "$comando_a_ejecutar")

  # Escribe la salida del comando en el archivo de bitácora
  escribir_bitacora "Se ejecutó el comando: ${comando_a_ejecutar}"
  escribir_bitacora "Salida del comando: ${salida_comando}"

  # Finaliza el script con éxito
  exit 0
}

# Ejecuta la función principal
main
```

Explicación del código:

* La primera línea del código indica que el script se ejecutará con el intérprete de comandos "/bin/bash".
* La segunda línea define una variable global llamada "DIRECTORIO_BASE" que se utilizará para almacenar los archivos de configuración y de bitácora.
* La tercera línea define una variable global llamada "ARCHIVO_CONFIGURACION" que se utilizará para almacenar el archivo de configuración.
* La cuarta línea define una variable global llamada "ARCHIVO_BITACORA" que se utilizará para almacenar el archivo de bitácora.
* La quinta línea define una función llamada "leer_configuracion()" que se utilizará para leer el archivo de configuración.
* La sexta línea define una función llamada "escribir_bitacora()" que se utilizará para escribir en el archivo de bitácora.
* La séptima línea define una función llamada "main()" que se utilizará como punto de entrada del script.
* La octava línea llama a la función "leer_configuracion()" para leer el archivo de configuración.
* La novena línea llama a la función "escribir_bitacora()" para escribir un mensaje en el archivo de bitácora.
* La décima línea ejecuta un comando especificado en la variable "comando_a_ejecutar".
* La undécima línea captura la salida del comando ejecutado en la variable "salida_comando".
* La duodécima línea llama a la función "escribir_bitacora()" para escribir la salida del comando ejecutado en el archivo de bitácora.
* La decimotercera línea finaliza el script con éxito.