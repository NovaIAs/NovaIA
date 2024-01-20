```
#!/bin/bash

# Variables globales

# Definimos el directorio actual como variable global
DIRECTORIO_ACTUAL=$(pwd)

# Función para mostrar el menú principal
function mostrarMenuPrincipal() {
  clear
  echo "Menú principal"
  echo "1. Crear un nuevo archivo"
  echo "2. Editar un archivo existente"
  echo "3. Eliminar un archivo"
  echo "4. Copiar un archivo"
  echo "5. Mover un archivo"
  echo "6. Mostrar el contenido de un directorio"
  echo "7. Salir"
  echo ""
  read -p "Elige una opción: " OPCION
}

# Función para crear un nuevo archivo
function crearArchivo() {
  clear
  echo "Crear un nuevo archivo"
  echo ""
  read -p "Introduce el nombre del archivo: " NOMBRE_ARCHIVO
  touch "$DIRECTORIO_ACTUAL/$NOMBRE_ARCHIVO"
  echo "Archivo creado correctamente."
}

# Función para editar un archivo existente
function editarArchivo() {
  clear
  echo "Editar un archivo existente"
  echo ""
  read -p "Introduce el nombre del archivo: " NOMBRE_ARCHIVO
  if [ -f "$DIRECTORIO_ACTUAL/$NOMBRE_ARCHIVO" ]; then
    nano "$DIRECTORIO_ACTUAL/$NOMBRE_ARCHIVO"
    echo "Archivo editado correctamente."
  else
    echo "No existe ningún archivo con ese nombre."
  fi
}

# Función para eliminar un archivo
function eliminarArchivo() {
  clear
  echo "Eliminar un archivo"
  echo ""
  read -p "Introduce el nombre del archivo: " NOMBRE_ARCHIVO
  if [ -f "$DIRECTORIO_ACTUAL/$NOMBRE_ARCHIVO" ]; then
    rm "$DIRECTORIO_ACTUAL/$NOMBRE_ARCHIVO"
    echo "Archivo eliminado correctamente."
  else
    echo "No existe ningún archivo con ese nombre."
  fi
}

# Función para copiar un archivo
function copiarArchivo() {
  clear
  echo "Copiar un archivo"
  echo ""
  read -p "Introduce el nombre del archivo: " NOMBRE_ARCHIVO
  if [ -f "$DIRECTORIO_ACTUAL/$NOMBRE_ARCHIVO" ]; then
    read -p "Introduce el nuevo nombre del archivo: " NUEVO_NOMBRE_ARCHIVO
    cp "$DIRECTORIO_ACTUAL/$NOMBRE_ARCHIVO" "$DIRECTORIO_ACTUAL/$NUEVO_NOMBRE_ARCHIVO"
    echo "Archivo copiado correctamente."
  else
    echo "No existe ningún archivo con ese nombre."
  fi
}

# Función para mover un archivo
function moverArchivo() {
  clear
  echo "Mover un archivo"
  echo ""
  read -p "Introduce el nombre del archivo: " NOMBRE_ARCHIVO
  if [ -f "$DIRECTORIO_ACTUAL/$NOMBRE_ARCHIVO" ]; then
    read -p "Introduce la nueva ubicación del archivo: " NUEVA_UBICACION
    mv "$DIRECTORIO_ACTUAL/$NOMBRE_ARCHIVO" "$NUEVA_UBICACION/$NOMBRE_ARCHIVO"
    echo "Archivo movido correctamente."
  else
    echo "No existe ningún archivo con ese nombre."
  fi
}

# Función para mostrar el contenido de un directorio
function mostrarContenidoDirectorio() {
  clear
  echo "Mostrar el contenido de un directorio"
  echo ""
  read -p "Introduce el directorio: " DIRECTORIO
  if [ -d "$DIRECTORIO" ]; then
    ls "$DIRECTORIO"
  else
    echo "No existe ningún directorio con ese nombre."
  fi
}

# Bucle principal

while true; do
  mostrarMenuPrincipal
  case "$OPCION" in
    1) crearArchivo ;;
    2) editarArchivo ;;
    3) eliminarArchivo ;;
    4) copiarArchivo ;;
    5) moverArchivo ;;
    6) mostrarContenidoDirectorio ;;
    7) exit ;;
    *) echo "Opción no válida." ;;
  esac
done
```

Explicación del código:

* El script comienza definiendo las variables globales, que son accesibles desde cualquier función del script. En este caso, la variable `DIRECTORIO_ACTUAL` se utiliza para almacenar el directorio actual.
* A continuación, se definen las funciones que se utilizarán en el script. Cada función realiza una tarea específica, como crear un nuevo archivo, editar un archivo existente, eliminar un archivo, copiar un archivo, mover un archivo y mostrar el contenido de un directorio.
* La función `mostrarMenuPrincipal()` muestra el menú principal al usuario y le permite elegir una opción.
* Las funciones `crearArchivo()`, `editarArchivo()`, `eliminarArchivo()`, `copiarArchivo()`, `moverArchivo()` y `mostrarContenidoDirectorio()` se encargan de realizar las tareas correspondientes a cada opción del menú principal.
* El bucle `while true` se utiliza para repetir el menú principal hasta que el usuario elija la opción `7` para salir del script.
* El comando `case "$OPCION" in` se utiliza para evaluar la opción elegida por el usuario y ejecutar la función correspondiente.
* El comando `exit` se utiliza para salir del script.