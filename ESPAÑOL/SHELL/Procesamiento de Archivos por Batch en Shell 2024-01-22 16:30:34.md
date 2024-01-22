```
#!/bin/bash

# Definir variables
archivos_procesados=0
lineas_procesadas=0
errores_procesados=0

# Función para procesar un archivo
procesar_archivo() {
  # Comprobar si el archivo existe
  if [ ! -f "$1" ]; then
    echo "El archivo $1 no existe."
    return 1
  fi

  # Contar el número de líneas del archivo
  lineas_archivo=$(wc -l < "$1")

  # Procesar cada línea del archivo
  while read linea; do
    # Incrementar el contador de líneas procesadas
    lineas_procesadas=$((lineas_procesadas + 1))

    # Procesar la línea
    # Aquí iría el código para procesar la línea

    # Comprobar si ha habido un error en el procesamiento
    if [ $? -ne 0 ]; then
      # Incrementar el contador de errores procesados
      errores_procesados=$((errores_procesados + 1))

      # Mostrar el error
      echo "Error al procesar la línea $lineas_procesadas del archivo $1."
    fi
  done < "$1"

  # Incrementar el contador de archivos procesados
  archivos_procesados=$((archivos_procesados + 1))
}

# Procesar todos los archivos de un directorio
procesar_directorio() {
  # Obtener la lista de archivos del directorio
  archivos=$(find "$1" -type f)

  # Procesar cada archivo
  for archivo in $archivos; do
    procesar_archivo "$archivo"
  done
}

# Mostrar el resumen del procesamiento
mostrar_resumen() {
  echo "Archivos procesados: $archivos_procesados"
  echo "Líneas procesadas: $lineas_procesadas"
  echo "Errores procesados: $errores_procesados"
}

# Procesar todos los archivos de un directorio
procesar_directorio "/ruta/al/directorio"

# Mostrar el resumen del procesamiento
mostrar_resumen
```

Explicación del código:

* La primera parte del código define las variables que se utilizarán en el script.
* La función `procesar_archivo()` procesa un archivo. Primero comprueba si el archivo existe. Si existe, cuenta el número de líneas del archivo y luego procesa cada línea del archivo. Si hay un error al procesar la línea, se incrementa el contador de errores procesados y se muestra el error.
* La función `procesar_directorio()` procesa todos los archivos de un directorio. Primero obtiene la lista de archivos del directorio y luego procesa cada archivo.
* La función `mostrar_resumen()` muestra el resumen del procesamiento. Muestra el número de archivos procesados, el número de líneas procesadas y el número de errores procesados.
* La última parte del código procesa todos los archivos del directorio `/ruta/al/directorio` y muestra el resumen del procesamiento.