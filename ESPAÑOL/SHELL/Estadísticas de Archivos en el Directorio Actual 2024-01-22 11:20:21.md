```
#!/bin/bash

# Declaración de variables
#
# La variable NUM_ARCHIVOS almacena el número de archivos en el directorio actual.
# La variable TOTAL_BYTES almacena el tamaño total de los archivos en bytes.
# La variable MAYORES_1MB almacena el número de archivos mayores de 1 megabyte.
# La variable NOMBRES_ARCHIVOS_MAYORES_1MB almacena los nombres de los archivos mayores de 1 megabyte.
# La variable DIRECTORIO_ACTUAL almacena el directorio actual.

NUM_ARCHIVOS=0
TOTAL_BYTES=0
MAYORES_1MB=0
NOMBRES_ARCHIVOS_MAYORES_1MB=""
DIRECTORIO_ACTUAL=$(pwd)

# Se recorren todos los archivos del directorio actual.
#
# Para cada archivo, se obtiene su tamaño en bytes y se suman al total.
# Si el tamaño del archivo es mayor de 1 megabyte, se incrementa el contador de archivos mayores de 1 megabyte y se añade el nombre del archivo a la lista de nombres de archivos mayores de 1 megabyte.
# Se incrementa el contador de archivos en 1.

for ARCHIVO in "$DIRECTORIO_ACTUAL"/*; do
    TAMANO_ARCHIVO=$(stat -c %s "$ARCHIVO")
    TOTAL_BYTES=$((TOTAL_BYTES + TAMANO_ARCHIVO))

    if [ $TAMANO_ARCHIVO -gt 1048576 ]; then
        MAYORES_1MB=$((MAYORES_1MB + 1))
        NOMBRES_ARCHIVOS_MAYORES_1MB="$NOMBRES_ARCHIVOS_MAYORES_1MB $ARCHIVO"
    fi

    NUM_ARCHIVOS=$((NUM_ARCHIVOS + 1))
done

# Se muestran los resultados.
#
# Se muestra el número de archivos en el directorio actual.
# Se muestra el tamaño total de los archivos en bytes.
# Se muestra el número de archivos mayores de 1 megabyte.
# Se muestra la lista de nombres de los archivos mayores de 1 megabyte.

echo "Número de archivos en el directorio actual: $NUM_ARCHIVOS"
echo "Tamaño total de los archivos en bytes: $TOTAL_BYTES"
echo "Número de archivos mayores de 1 megabyte: $MAYORES_1MB"
echo "Nombres de los archivos mayores de 1 megabyte: $NOMBRES_ARCHIVOS_MAYORES_1MB"
```

Este código recorre todos los archivos del directorio actual y calcula el número de archivos, el tamaño total de los archivos en bytes, el número de archivos mayores de 1 megabyte y la lista de nombres de los archivos mayores de 1 megabyte. Luego, muestra los resultados en la consola.