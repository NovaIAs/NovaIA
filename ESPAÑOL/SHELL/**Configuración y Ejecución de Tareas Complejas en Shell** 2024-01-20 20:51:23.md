```shell
#!/bin/bash

# DECLARACIÓN DE VARIABLES
# -------------------------
fichero_cfg="$HOME/.fichero_cfg"
fichero_tmp="/tmp/fichero_tmp"
fichero_log="/var/log/fichero.log"
fichero_pid="/var/run/fichero.pid"
email_contacto="contacto@dominio.com"
fecha_hora_actual=$(date +%Y-%m-%d\ %H:%M:%S)

# CREACIÓN DEL FICHERO DE CONFIGURACIÓN
# ----------------------------------------
if [ ! -f "$fichero_cfg" ]; then
  echo "Configurando el fichero de configuración..."
  echo "email_contacto=$email_contacto" > "$fichero_cfg"
  echo "fichero_log=$fichero_log" >> "$fichero_cfg"
  echo "fichero_pid=$fichero_pid" >> "$fichero_cfg"
  echo "fichero_tmp=$fichero_tmp" >> "$fichero_cfg"
  echo "Configuración completada."
fi

# CARGA DEL FICHERO DE CONFIGURACIÓN
# ------------------------------------
source "$fichero_cfg"

# FUNCIÓN PARA ENVIAR UN CORREO ELECTRÓNICO
# ---------------------------------------------
function enviar_correo() {
  asunto=$1
  cuerpo=$2
  echo "$cuerpo" | mail -s "$asunto" "$email_contacto"
}

# FUNCIÓN PARA REGISTRAR UN MENSAJE EN EL LOG
# ---------------------------------------------
function registrar_log() {
  mensaje=$1
  echo "$fecha_hora_actual: $mensaje" >> "$fichero_log"
}

# FUNCIÓN PRINCIPAL
# -----------------
function main() {
  # COMPROBACIÓN DE LA EXISTENCIA DEL PID
  # ---------------------------------------
  if [ -f "$fichero_pid" ]; then
    registrar_log "El proceso ya se está ejecutando."
    enviar_correo "Proceso ya en ejecución" "El proceso ya se está ejecutando."
    exit 1
  fi

  # CREACIÓN DEL PID
  # -----------------
  echo $$ > "$fichero_pid"

  # EJECUCIÓN DE LA TAREA
  # ----------------------
  # Aquí se ejecuta la tarea que se desea realizar.
  # Por ejemplo, la llamada a un script externo:
  /ruta/a/script.sh

  # ELIMINACIÓN DEL PID
  # --------------------
  rm "$fichero_pid"
}

# LLAMADA A LA FUNCIÓN PRINCIPAL
# ---------------------------------
main

```

Explicación:

* Este código es un script de shell complejo que puede realizar una variedad de tareas.
* La primera parte del código declara las variables que se utilizarán en el script.
* La segunda parte del código crea el fichero de configuración, si no existe.
* La tercera parte del código carga el fichero de configuración.
* La cuarta parte del código define una función para enviar un correo electrónico.
* La quinta parte del código define una función para registrar un mensaje en el log.
* La sexta parte del código define la función principal.
* La séptima parte del código comprueba si el proceso ya se está ejecutando.
* La octava parte del código crea el PID.
* La novena parte del código ejecuta la tarea deseada.
* La décima parte del código elimina el PID.
* La undécima parte del código llama a la función principal.