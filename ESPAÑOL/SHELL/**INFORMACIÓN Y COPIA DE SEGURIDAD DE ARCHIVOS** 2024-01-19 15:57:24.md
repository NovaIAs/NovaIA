```shell
#!/bin/bash

# Definir variables globales
directorio_actual=$(pwd)
lista_archivos=$(ls)
numero_archivos=$(ls -1 | wc -l)
extensiones_archivos=$(ls -1 | rev | cut -d'.' -f1 | rev | sort | uniq)
directorios=$(find -mindepth 1 -maxdepth 1 -type d)

# Mostrar información general de la carpeta actual
echo "Directorio actual: $directorio_actual"
echo "Número de archivos: $numero_archivos"
echo "Extensiones de archivos:"
for extension in $extensiones_archivos; do
  echo " - $extension"
done

# Mostrar información detallada de cada archivo en la carpeta actual
for archivo in $lista_archivos; do
  echo "Nombre de archivo: $archivo"
  echo "Tamaño del archivo: $(stat -c %s $archivo) bytes"
  echo "Fecha de última modificación: $(stat -c %y $archivo)"
done

# Crear un nuevo directorio llamado "copia_seguridad"
mkdir copia_seguridad

# Copiar todos los archivos de la carpeta actual al directorio "copia_seguridad"
cp -r * copia_seguridad/

# Mostrar el contenido del directorio "copia_seguridad"
echo "Contenido del directorio copia_seguridad:"
ls copia_seguridad/

# Eliminar el directorio "copia_seguridad"
rm -r copia_seguridad

# Salir del script
exit 0
```

Este script se encarga de mostrar información general y detallada de la carpeta actual, crear un directorio de copia de seguridad, copiar todos los archivos de la carpeta actual al directorio de copia de seguridad y luego eliminar el directorio de copia de seguridad.

El script está comentado para que sea más fácil de entender y está compuesto por las siguientes partes:

* **Definición de variables globales:** Estas variables se utilizan a lo largo del script para almacenar información sobre la carpeta actual y sus archivos.
* **Mostrar información general de la carpeta actual:** Esta parte del script muestra información general sobre la carpeta actual, incluyendo el nombre de la carpeta, el número de archivos y las extensiones de los archivos.
* **Mostrar información detallada de cada archivo en la carpeta actual:** Esta parte del script muestra información detallada sobre cada archivo en la carpeta actual, incluyendo el nombre del archivo, el tamaño del archivo y la fecha de la última modificación.
* **Crear un nuevo directorio llamado "copia_seguridad":** Esta parte del script crea un nuevo directorio llamado "copia_seguridad" en la carpeta actual.
* **Copiar todos los archivos de la carpeta actual al directorio "copia_seguridad":** Esta parte del script copia todos los archivos de la carpeta actual al directorio "copia_seguridad".
* **Mostrar el contenido del directorio "copia_seguridad":** Esta parte del script muestra el contenido del directorio "copia_seguridad".
* **Eliminar el directorio "copia_seguridad":** Esta parte del script elimina el directorio "copia_seguridad".
* **Salir del script:** Esta parte del script sale del script con el código de salida 0, que indica que el script se ha ejecutado correctamente.