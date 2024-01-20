```shell
#!/bin/bash

# Definición de variables
DIRECTORIO_RAÍZ="/home/usuario"
ARCHIVO_CONFIGURACIÓN="${DIRECTORIO_RAÍZ}/.configuración"
ARCHIVO_BITÁCORA="${DIRECTORIO_RAÍZ}/bitácora.log"

# Función para registrar mensajes en la bitácora
function registrar_mensaje() {
  local mensaje="$1"
  local fecha=$(date +%Y-%m-%d %H:%M:%S)
  echo "${fecha}: ${mensaje}" >> "${ARCHIVO_BITÁCORA}"
}

# Función para obtener la configuración
function obtener_configuración() {
  local clave="$1"
  local valor=$(grep "^${clave}=" "${ARCHIVO_CONFIGURACIÓN}" | cut -d'=' -f2)
  echo "${valor}"
}

# Función para establecer la configuración
function establecer_configuración() {
  local clave="$1"
  local valor="$2"
  sed -i "s/^${clave}=.*/${clave}=${valor}/" "${ARCHIVO_CONFIGURACIÓN}"
}

# Función para listar los archivos en un directorio
function listar_archivos() {
  local directorio="$1"
  find "${directorio}" -type f -print
}

# Función para copiar archivos
function copiar_archivos() {
  local origen="$1"
  local destino="$2"
  cp -r "${origen}" "${destino}"
}

# Función para eliminar archivos
function eliminar_archivos() {
  local archivos="$1"
  rm -rf "${archivos}"
}

# Función para crear un archivo comprimido
function comprimir_archivo() {
  local archivo="$1"
  local destino="$2"
  tar -czvf "${destino}" "${archivo}"
}

# Función para descomprimir un archivo
function descomprimir_archivo() {
  local archivo="$1"
  local destino="$2"
  tar -xzvf "${archivo}" -C "${destino}"
}

# Función para enviar un correo electrónico
function enviar_correo() {
  local destinatario="$1"
  local asunto="$2"
  local mensaje="$3"
  echo "${mensaje}" | mail -s "${asunto}" "${destinatario}"
}

# Función principal
function main() {
  # Registrar un mensaje en la bitácora
  registrar_mensaje "Inicio del script"

  # Obtener la configuración
  local puerto=$(obtener_configuración "puerto")
  local dirección_ip=$(obtener_configuración "dirección_ip")

  # Establecer la configuración
  establecer_configuración "puerto" "8080"

  # Listar los archivos en un directorio
  local archivos=$(listar_archivos "${DIRECTORIO_RAÍZ}")

  # Copiar archivos
  copiar_archivos "${DIRECTORIO_RAÍZ}/archivos_a_copiar" "${DIRECTORIO_RAÍZ}/archivos_copiados"

  # Eliminar archivos
  eliminar_archivos "${DIRECTORIO_RAÍZ}/archivos_a_eliminar"

  # Crear un archivo comprimido
  comprimir_archivo "${DIRECTORIO_RAÍZ}/archivos_a_comprimir" "${DIRECTORIO_RAÍZ}/archivos_comprimidos.tar.gz"

  # Descomprimir un archivo
  descomprimir_archivo "${DIRECTORIO_RAÍZ}/archivos_a_descomprimir.tar.gz" "${DIRECTORIO_RAÍZ}/archivos_descomprimidos"

  # Enviar un correo electrónico
  enviar_correo "destinatario@ejemplo.com" "Asunto del correo" "Mensaje del correo"

  # Registrar un mensaje en la bitácora
  registrar_mensaje "Fin del script"
}

# Llamar a la función principal
main
```

Explicación del código:

1. **Definición de variables:** Se definen las variables necesarias para el script, incluyendo el directorio raíz, el archivo de configuración y el archivo de bitácora.
2. **Función para registrar mensajes en la bitácora:** Esta función registra un mensaje en el archivo de bitácora, incluyendo la fecha y la hora.
3. **Función para obtener la configuración:** Esta función obtiene el valor de una clave de configuración del archivo de configuración.
4. **Función para establecer la configuración:** Esta función establece el valor de una clave de configuración en el archivo de configuración.
5. **Función para listar los archivos en un directorio:** Esta función lista todos los archivos en un directorio especificado.
6. **Función para copiar archivos:** Esta función copia archivos de un directorio a otro.
7. **Función para eliminar archivos:** Esta función elimina archivos o directorios especificados.
8. **Función para crear un archivo comprimido:** Esta función comprime un archivo o directorio en un archivo .tar.gz.
9. **Función para descomprimir un archivo:** Esta función descomprime un archivo .tar.gz en un directorio especificado.
10. **Función para enviar un correo electrónico:** Esta función envía un correo electrónico a un destinatario especificado, con un asunto y un mensaje especificados.
11. **Función principal:** La función principal llama a las otras funciones para realizar las tareas necesarias, como registrar mensajes en la bitácora, obtener y establecer la configuración, listar archivos, copiar archivos, eliminar archivos, crear archivos comprimidos, descomprimir archivos y enviar correos electrónicos.
12. **Llamada a la función principal:** La función principal se llama al final del script para ejecutar el script.