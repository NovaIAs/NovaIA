```shell
#!/bin/bash

# Definición de variables
directorio_actual=$(pwd)
fichero_temporal=$(mktemp)

# Función para mostrar el menú
function mostrar_menu() {
  clear
  echo "MENÚ PRINCIPAL"
  echo "1. Listar archivos y directorios"
  echo "2. Crear un directorio"
  echo "3. Eliminar un directorio"
  echo "4. Copiar un archivo"
  echo "5. Mover un archivo"
  echo "6. Salir"
  echo ""
  echo -n "Seleccione una opción: "
}

# Función para listar archivos y directorios
function listar_archivos_directorios() {
  clear
  echo "LISTADO DE ARCHIVOS Y DIRECTORIOS"
  echo ""
  ls -l
  echo ""
  echo "Presione cualquier tecla para continuar..."
  read -n 1
}

# Función para crear un directorio
function crear_directorio() {
  clear
  echo "CREAR UN DIRECTORIO"
  echo ""
  echo -n "Introduzca el nombre del directorio: "
  read nombre_directorio

  if [[ ! -d "$nombre_directorio" ]]; then
    mkdir "$nombre_directorio"
    echo ""
    echo "El directorio se ha creado correctamente."
  else
    echo ""
    echo "El directorio ya existe."
  fi

  echo ""
  echo "Presione cualquier tecla para continuar..."
  read -n 1
}

# Función para eliminar un directorio
function eliminar_directorio() {
  clear
  echo "ELIMINAR UN DIRECTORIO"
  echo ""
  echo -n "Introduzca el nombre del directorio: "
  read nombre_directorio

  if [[ -d "$nombre_directorio" ]]; then
    rm -r "$nombre_directorio"
    echo ""
    echo "El directorio se ha eliminado correctamente."
  else
    echo ""
    echo "El directorio no existe."
  fi

  echo ""
  echo "Presione cualquier tecla para continuar..."
  read -n 1
}

# Función para copiar un archivo
function copiar_archivo() {
  clear
  echo "COPIAR UN ARCHIVO"
  echo ""
  echo -n "Introduzca el nombre del archivo a copiar: "
  read nombre_archivo_copiar

  if [[ -f "$nombre_archivo_copiar" ]]; then
    echo ""
    echo -n "Introduzca el nombre del archivo de destino: "
    read nombre_archivo_destino

    cp "$nombre_archivo_copiar" "$nombre_archivo_destino"
    echo ""
    echo "El archivo se ha copiado correctamente."
  else
    echo ""
    echo "El archivo a copiar no existe."
  fi

  echo ""
  echo "Presione cualquier tecla para continuar..."
  read -n 1
}

# Función para mover un archivo
function mover_archivo() {
  clear
  echo "MOVER UN ARCHIVO"
  echo ""
  echo -n "Introduzca el nombre del archivo a mover: "
  read nombre_archivo_mover

  if [[ -f "$nombre_archivo_mover" ]]; then
    echo ""
    echo -n "Introduzca el nombre del archivo de destino: "
    read nombre_archivo_destino

    mv "$nombre_archivo_mover" "$nombre_archivo_destino"
    echo ""
    echo "El archivo se ha movido correctamente."
  else
    echo ""
    echo "El archivo a mover no existe."
  fi

  echo ""
  echo "Presione cualquier tecla para continuar..."
  read -n 1
}

# Función para salir del programa
function salir() {
  clear
  echo "SALIENDO DEL PROGRAMA..."
  echo ""
  exit 0
}

# Bucle principal del programa
while true; do
  mostrar_menu
  read opcion

  case $opcion in
    1)
      listar_archivos_directorios
      ;;
    2)
      crear_directorio
      ;;
    3)
      eliminar_directorio
      ;;
    4)
      copiar_archivo
      ;;
    5)
      mover_archivo
      ;;
    6)
      salir
      ;;
    *)
      echo ""
      echo "Opción no válida."
      echo ""
      echo "Presione cualquier tecla para continuar..."
      read -n 1
      ;;
  esac
done
```

Explicación del código:

* El programa define una serie de funciones que se utilizan para llevar a cabo las diferentes tareas que puede realizar el programa.
* La función `mostrar_menu()` muestra el menú de opciones al usuario y le pide que introduzca una opción.
* La función `listar_archivos_directorios()` lista los archivos y directorios del directorio actual.
* La función `crear_directorio()` crea un nuevo directorio en el directorio actual.
* La función `eliminar_directorio()` elimina un directorio del directorio actual.
* La función `copiar_archivo()` copia un archivo de un directorio a otro.
* La función `mover_archivo()` mueve un archivo de un directorio a otro.
* La función `salir()` sale del programa.
* El bucle `while true` se utiliza para que el programa se ejecute continuamente hasta que el usuario introduzca la opción de salir.
* Dentro del bucle `while true`, el programa llama a la función `mostrar_menu()` para mostrar el menú de opciones al usuario.
* El programa lee la opción introducida por el usuario y llama a la función correspondiente para llevar a cabo la tarea seleccionada.
* Si el usuario introduce una opción no válida, el programa muestra un mensaje de error y pide al usuario que introduzca una opción válida.