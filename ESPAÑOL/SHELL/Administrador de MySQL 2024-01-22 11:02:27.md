```shell
#!/bin/bash

# Definición de constantes
MENSAJE_MENU_PRINCIPAL="Escoge una opción:"
OPCION_1="**1.** Iniciar servicio MySQL"
OPCION_2="**2.** Detener servicio MySQL"
OPCION_3="**3.** Reiniciar servicio MySQL"
OPCION_4="**4.** Ver estado de MySQL"
OPCION_5="**5.** Crear base de datos"
OPCION_6="**6.** Eliminar base de datos"
OPCION_7="**7.** Crear usuario MySQL"
OPCION_8="**8.** Eliminar usuario MySQL"
OPCION_9="**9.** Otorgar permisos de usuario MySQL"
OPCION_10="**10.** Revocar permisos de usuario MySQL"
OPCION_11="**11.** Importar base de datos"
OPCION_12="**12.** Exportar base de datos"
OPCION_13="**13.** Ejecutar script SQL"
OPCION_14="**14.** Salir"

# Función para mostrar el menú principal
function mostrar_menu_principal() {
  echo "$MENSAJE_MENU_PRINCIPAL"
  echo "$OPCION_1"
  echo "$OPCION_2"
  echo "$OPCION_3"
  echo "$OPCION_4"
  echo "$OPCION_5"
  echo "$OPCION_6"
  echo "$OPCION_7"
  echo "$OPCION_8"
  echo "$OPCION_9"
  echo "$OPCION_10"
  echo "$OPCION_11"
  echo "$OPCION_12"
  echo "$OPCION_13"
  echo "$OPCION_14"
}

# Función para obtener la opción seleccionada por el usuario
function obtener_opcion_seleccionada() {
  read -p "Opción: " opcion_seleccionada
}

# Función para iniciar el servicio MySQL
function iniciar_servicio_mysql() {
  echo "Iniciando servicio MySQL..."
  service mysql start
  echo "Servicio MySQL iniciado."
}

# Función para detener el servicio MySQL
function detener_servicio_mysql() {
  echo "Deteniendo servicio MySQL..."
  service mysql stop
  echo "Servicio MySQL detenido."
}

# Función para reiniciar el servicio MySQL
function reiniciar_servicio_mysql() {
  echo "Reiniciando servicio MySQL..."
  service mysql restart
  echo "Servicio MySQL reiniciado."
}

# Función para ver el estado del servicio MySQL
function ver_estado_mysql() {
  echo "Estado del servicio MySQL:"
  service mysql status
}

# Función para crear una base de datos
function crear_base_datos() {
  echo "Nombre de la base de datos:"
  read nombre_base_datos
  echo "Creando base de datos '$nombre_base_datos'..."
  mysql -e "CREATE DATABASE $nombre_base_datos"
  echo "Base de datos '$nombre_base_datos' creada."
}

# Función para eliminar una base de datos
function eliminar_base_datos() {
  echo "Nombre de la base de datos:"
  read nombre_base_datos
  echo "Eliminando base de datos '$nombre_base_datos'..."
  mysql -e "DROP DATABASE $nombre_base_datos"
  echo "Base de datos '$nombre_base_datos' eliminada."
}

# Función para crear un usuario MySQL
function crear_usuario_mysql() {
  echo "Nombre del usuario:"
  read nombre_usuario
  echo "Contraseña del usuario:"
  read contrasena_usuario
  echo "Creando usuario '$nombre_usuario'..."
  mysql -e "CREATE USER '$nombre_usuario'@'localhost' IDENTIFIED BY '$contrasena_usuario'"
  echo "Usuario '$nombre_usuario' creado."
}

# Función para eliminar un usuario MySQL
function eliminar_usuario_mysql() {
  echo "Nombre del usuario:"
  read nombre_usuario
  echo "Eliminando usuario '$nombre_usuario'..."
  mysql -e "DROP USER '$nombre_usuario'@'localhost'"
  echo "Usuario '$nombre_usuario' eliminado."
}

# Función para otorgar permisos de usuario MySQL
function otorgar_permisos_usuario_mysql() {
  echo "Nombre del usuario:"
  read nombre_usuario
  echo "Nombre de la base de datos:"
  read nombre_base_datos
  echo "Permisos a otorgar (lectura, escritura, administración):"
  read permisos
  echo "Otorgando permisos '$permisos' al usuario '$nombre_usuario' en la base de datos '$nombre_base_datos'..."
  mysql -e "GRANT $permisos ON $nombre_base_datos.* TO '$nombre_usuario'@'localhost'"
  echo "