```shell
#!/bin/bash

# Definición de variables
DIRECTORIO_ACTUAL=$(pwd)
ARCHIVO_TEMPORAL="/tmp/archivo_temporal"
ARCHIVO_LOG="/var/log/script.log"

# Captura de opciones de línea de comandos
while getopts ":a:b:c" OPCION; do
  case ${OPCION} in
    a)
      OPCION_A="${OPTARG}"
      ;;
    b)
      OPCION_B="${OPTARG}"
      ;;
    c)
      OPCION_C=true
      ;;
    *)
      echo "Opción no reconocida: ${OPCION}"
      exit 1
      ;;
  esac
done

# Validación de las opciones de línea de comandos
if [ -z "${OPCION_A}" ]; then
  echo "Falta la opción -a"
  exit 1
fi

# Creación de un archivo temporal
echo "Creando archivo temporal en ${ARCHIVO_TEMPORAL}"
touch "${ARCHIVO_TEMPORAL}"

# Escritura en el archivo temporal
echo "Escribiendo datos en el archivo temporal"
echo "Dato 1: ${OPCION_A}" >> "${ARCHIVO_TEMPORAL}"
echo "Dato 2: ${OPCION_B}" >> "${ARCHIVO_TEMPORAL}"

# Comprobación de la existencia del archivo log
if [ -f "${ARCHIVO_LOG}" ]; then
  echo "El archivo de registro ya existe"
else
  echo "Creando archivo de registro en ${ARCHIVO_LOG}"
  touch "${ARCHIVO_LOG}"
fi

# Escritura en el archivo log
echo "Escribiendo datos en el archivo de registro"
echo "Información: ejecución del script en ${DIRECTORIO_ACTUAL}" >> "${ARCHIVO_LOG}"

# Condición para la opción -c
if [ ${OPCION_C} ]; then
  echo "Se ha especificado la opción -c"
fi

# Eliminación del archivo temporal
echo "Eliminando archivo temporal en ${ARCHIVO_TEMPORAL}"
rm "${ARCHIVO_TEMPORAL}"

# Finalización del script
echo "Finalización del script"

```

Explicación del código:

1. **Definición de variables:** Se definen varias variables, incluyendo el directorio actual, la ruta de un archivo temporal, la ruta de un archivo de registro y una variable booleana para controlar una opción de línea de comandos.

2. **Captura de opciones de línea de comandos:** El script utiliza getopt para capturar las opciones de línea de comandos. Se reconocen las opciones `-a`, `-b` y `-c`.

3. **Validación de las opciones de línea de comandos:** Se comprueba si se ha especificado la opción `-a`. Si no se ha especificado, se muestra un mensaje de error y se sale del script.

4. **Creación de un archivo temporal:** Se crea un archivo temporal en la ruta especificada en la variable `ARCHIVO_TEMPORAL`.

5. **Escritura en el archivo temporal:** Se escriben datos en el archivo temporal, incluyendo el valor de las opciones `-a` y `-b`.

6. **Comprobación de la existencia del archivo log:** Se comprueba si existe el archivo de registro en la ruta especificada en la variable `ARCHIVO_LOG`. Si no existe, se crea un nuevo archivo de registro.

7. **Escritura en el archivo log:** Se escriben datos en el archivo de registro, incluyendo información sobre la ejecución del script.

8. **Condición para la opción `-c`:** Se comprueba si se ha especificado la opción `-c`. Si se ha especificado, se muestra un mensaje indicando que la opción se ha especificado.

9. **Eliminación del archivo temporal:** Se elimina el archivo temporal que se creó anteriormente.

10. **Finalización del script:** Se muestra un mensaje indicando que el script ha finalizado su ejecución.