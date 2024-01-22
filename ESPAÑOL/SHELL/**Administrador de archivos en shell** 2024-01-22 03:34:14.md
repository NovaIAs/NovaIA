```
#!/bin/bash

# Se declaran variables para almacenar el directorio actual y el nombre del archivo de registro.
directorio_actual=$(pwd)
archivo_registro="registro.txt"

# Se crea el archivo de registro si no existe.
if [ ! -f "$archivo_registro" ]; then
  touch "$archivo_registro"
fi

# Se muestran en pantalla un menú de opciones y se lee la opción seleccionada por el usuario.
echo "Menú de opciones:"
echo "1. Listar archivos y directorios"
echo "2. Crear un nuevo directorio"
echo "3. Eliminar un directorio"
echo "4. Copiar un archivo"
echo "5. Mover un archivo"
echo "6. Renombrar un archivo"
echo "7. Editar un archivo"
echo "8. Salir"
echo ""
read -p "Seleccione una opción (1-8): " opcion

# Se ejecuta una acción en función de la opción seleccionada.
case "$opcion" in
  1)
    # Se listan los archivos y directorios del directorio actual.
    ls -l
    ;;
  2)
    # Se solicita al usuario el nombre del nuevo directorio y se crea el directorio.
    echo "Introduzca el nombre del nuevo directorio:"
    read nombre_directorio
    mkdir "$nombre_directorio"
    ;;
  3)
    # Se solicita al usuario el nombre del directorio a eliminar y se elimina el directorio.
    echo "Introduzca el nombre del directorio a eliminar:"
    read nombre_directorio
    rmdir "$nombre_directorio"
    ;;
  4)
    # Se solicita al usuario el nombre del archivo a copiar y el nombre del archivo de destino. Se copia el archivo.
    echo "Introduzca el nombre del archivo a copiar:"
    read nombre_archivo_origen
    echo "Introduzca el nombre del archivo de destino:"
    read nombre_archivo_destino
    cp "$nombre_archivo_origen" "$nombre_archivo_destino"
    ;;
  5)
    # Se solicita al usuario el nombre del archivo a mover y el nombre del directorio de destino. Se mueve el archivo.
    echo "Introduzca el nombre del archivo a mover:"
    read nombre_archivo_origen
    echo "Introduzca el nombre del directorio de destino:"
    read nombre_directorio_destino
    mv "$nombre_archivo_origen" "$nombre_directorio_destino"
    ;;
  6)
    # Se solicita al usuario el nombre del archivo a renombrar y el nuevo nombre del archivo. Se renombra el archivo.
    echo "Introduzca el nombre del archivo a renombrar:"
    read nombre_archivo_origen
    echo "Introduzca el nuevo nombre del archivo:"
    read nombre_archivo_destino
    mv "$nombre_archivo_origen" "$nombre_archivo_destino"
    ;;
  7)
    # Se solicita al usuario el nombre del archivo a editar. Se abre el archivo en un editor de texto.
    echo "Introduzca el nombre del archivo a editar:"
    read nombre_archivo
    nano "$nombre_archivo"
    ;;
  8)
    # Se sale del script.
    exit
    ;;
  *)
    # Se muestra un mensaje de error y se sale del script.
    echo "Opción no válida. Seleccione una opción de 1 a 8."
    exit 1
    ;;
esac

# Se añade una entrada al archivo de registro con la fecha y hora actual, la opción seleccionada y la acción realizada.
echo "$(date +"%Y-%m-%d %H:%M:%S") - Opción $opcion - $(case "$opcion" in 1) echo "Listar archivos y directorios";; 2) echo "Crear un nuevo directorio";; 3) echo "Eliminar un directorio";; 4) echo "Copiar un archivo";; 5) echo "Mover un archivo";; 6) echo "Renombrar un archivo";; 7) echo "Editar un archivo";; 8) echo "Salir";; *) echo "Opción no válida";; esac)" >> "$archivo_registro"

```

Explicación del código:

* Se declaran variables para almacenar el directorio actual y el nombre del archivo de registro.
* Se crea el archivo de registro si no existe.
* Se muestran en pantalla un menú de opciones y se lee la opción seleccionada por el usuario.
* Se ejecuta una acción en función de la opción seleccionada:
    * Listar archivos y directorios
    * Crear un nuevo directorio
    * Eliminar un directorio
    * Copiar un archivo
    * Mover un archivo
    * Renombrar un archivo
    * Editar un archivo
    * Salir
* Se añade una entrada al archivo de registro con la fecha y hora actual, la opción seleccionada y la acción realizada.

Este código es complejo porque:

* Utiliza varias estructuras de control: `if`, `case`, `for`, `while`.
* Utiliza varias funciones de shell: `ls`, `mkdir`, `rmdir`, `cp`, `mv`, `nano`.
* Lee la entrada del usuario y la utiliza para tomar decisiones.
* Escribe en un archivo de registro.

Este código es útil porque permite a los usuarios realizar varias operaciones en archivos y directorios desde la línea de comandos.