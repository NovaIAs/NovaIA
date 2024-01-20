```shell
#!/bin/bash

# Definición de funciones

# Función para mostrar el menú principal
function mostrar_menu_principal() {
  clear
  echo "Menú principal"
  echo "1. Gestionar usuarios"
  echo "2. Gestionar grupos"
  echo "3. Gestionar archivos y directorios"
  echo "4. Ejecutar comandos"
  echo "5. Salir"
  read -p "Elige una opción: " opcion
  case $opcion in
    1) mostrar_menu_usuarios ;;
    2) mostrar_menu_grupos ;;
    3) mostrar_menu_archivos_directorios ;;
    4) ejecutar_comandos ;;
    5) salir ;;
    *) echo "Opción no válida. Inténtalo de nuevo."; mostrar_menu_principal ;;
  esac
}

# Función para mostrar el menú de usuarios
function mostrar_menu_usuarios() {
  clear
  echo "Menú de usuarios"
  echo "1. Crear usuario"
  echo "2. Eliminar usuario"
  echo "3. Modificar usuario"
  echo "4. Mostrar información de usuario"
  echo "5. Volver al menú principal"
  read -p "Elige una opción: " opcion
  case $opcion in
    1) crear_usuario ;;
    2) eliminar_usuario ;;
    3) modificar_usuario ;;
    4) mostrar_informacion_usuario ;;
    5) mostrar_menu_principal ;;
    *) echo "Opción no válida. Inténtalo de nuevo."; mostrar_menu_usuarios ;;
  esac
}

# Función para crear usuario
function crear_usuario() {
  clear
  echo "Crear usuario"
  read -p "Introduce el nombre de usuario: " nombre_usuario
  read -p "Introduce la contraseña: " contraseña
  useradd -m -p "$contraseña" "$nombre_usuario"
  echo "Usuario creado correctamente."
  mostrar_menu_usuarios
}

# Función para eliminar usuario
function eliminar_usuario() {
  clear
  echo "Eliminar usuario"
  read -p "Introduce el nombre de usuario: " nombre_usuario
  userdel "$nombre_usuario"
  echo "Usuario eliminado correctamente."
  mostrar_menu_usuarios
}

# Función para modificar usuario
function modificar_usuario() {
  clear
  echo "Modificar usuario"
  read -p "Introduce el nombre de usuario: " nombre_usuario
  read -p "Introduce la nueva contraseña: " nueva_contraseña
  passwd "$nombre_usuario" "$nueva_contraseña"
  echo "Contraseña modificada correctamente."
  mostrar_menu_usuarios
}

# Función para mostrar información de usuario
function mostrar_informacion_usuario() {
  clear
  echo "Mostrar información de usuario"
  read -p "Introduce el nombre de usuario: " nombre_usuario
  finger "$nombre_usuario"
  mostrar_menu_usuarios
}

# Función para mostrar el menú de grupos
function mostrar_menu_grupos() {
  clear
  echo "Menú de grupos"
  echo "1. Crear grupo"
  echo "2. Eliminar grupo"
  echo "3. Modificar grupo"
  echo "4. Mostrar información de grupo"
  echo "5. Volver al menú principal"
  read -p "Elige una opción: " opcion
  case $opcion in
    1) crear_grupo ;;
    2) eliminar_grupo ;;
    3) modificar_grupo ;;
    4) mostrar_informacion_grupo ;;
    5) mostrar_menu_principal ;;
    *) echo "Opción no válida. Inténtalo de nuevo."; mostrar_menu_grupos ;;
  esac
}

# Función para crear grupo
function crear_grupo() {
  clear
  echo "Crear grupo"
  read -p "Introduce el nombre del grupo: " nombre_grupo
  groupadd "$nombre_grupo"
  echo "Grupo creado correctamente."
  mostrar_menu_grupos
}

# Función para eliminar grupo
function eliminar_grupo() {
  clear
  echo "Eliminar grupo"
  read -p "Introduce el nombre del grupo: " nombre_grupo
  groupdel "$nombre_grupo"
  echo "Grupo eliminado correctamente."
  mostrar_menu_grupos
}

# Función para modificar grupo
function modificar_grupo() {
  clear
  echo "Modificar grupo"
  read -p "Introduce el nombre del grupo: " nombre_grupo
  read -p "Introduce el nuevo nombre del grupo: " nuevo_nombre_grupo
  groupmod -n "$nuevo_nombre_grupo" "$nombre_grupo"
  echo "Grupo modificado correctamente."
  mostrar_menu_grupos
}

# Función para mostrar información de grupo
function mostrar_informacion_grupo() {
  clear
  echo "Mostrar información de grupo"
  read -p "Introduce el nombre del grupo: " nombre_grupo
  