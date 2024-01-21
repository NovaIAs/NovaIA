```
#!/bin/bash

# Función para mostrar el uso del script
function mostrar_uso {
  echo "Uso: $0 <opción> <argumentos>"
  echo ""
  echo "Opciones:"
  echo "  -a, --archivo    Especifica el archivo a procesar"
  echo "  -d, --directorio  Especifica el directorio a procesar"
  echo "  -h, --ayuda    Muestra este mensaje de ayuda"
  echo ""
}

# Función para procesar un archivo
function procesar_archivo {
  # Obtener el nombre del archivo
  archivo="$1"

  # Comprobar si el archivo existe
  if [ ! -f "$archivo" ]; then
    echo "Error: el archivo $archivo no existe"
    exit 1
  fi

  # Procesar el archivo
  cat "$archivo" | grep "palabra clave" >> resultados.txt
}

# Función para procesar un directorio
function procesar_directorio {
  # Obtener el directorio
  directorio="$1"

  # Comprobar si el directorio existe
  if [ ! -d "$directorio" ]; then
    echo "Error: el directorio $directorio no existe"
    exit 1
  fi

  # Procesar el directorio
  find "$directorio" -type f -name "*.txt" -exec procesar_archivo {} \;
}

# Obtener la opción y los argumentos
opcion="$1"
argumento="$2"

# Comprobar la opción
case "$opcion" in
  -a|--archivo)
    procesar_archivo "$argumento"
    ;;
  -d|--directorio)
    procesar_directorio "$argumento"
    ;;
  -h|--ayuda)
    mostrar_uso
    exit 0
    ;;
  *)
    echo "Error: opción no válida '$opcion'"
    mostrar_uso
    exit 1
    ;;
esac
```

Este código es un script de shell que procesa un archivo o un directorio de archivos en busca de una palabra clave específica. El script tiene las siguientes características:

* **Opciones:** El script tiene tres opciones:
    * `-a`, `--archivo`: especifica el archivo a procesar
    * `-d`, `--directorio`: especifica el directorio a procesar
    * `-h`, `--ayuda`: muestra el mensaje de ayuda
* **Funciones:** El script tiene tres funciones:
    * `mostrar_uso`: muestra el mensaje de ayuda
    * `procesar_archivo`: procesa un archivo en busca de una palabra clave específica
    * `procesar_directorio`: procesa un directorio de archivos en busca de una palabra clave específica
* **Uso:** El script se puede utilizar de la siguiente manera:
    * `./script.sh -a archivo.txt`: procesa el archivo `archivo.txt` en busca de una palabra clave específica
    * `./script.sh -d directorio`: procesa todos los archivos en el directorio `directorio` en busca de una palabra clave específica
    * `./script.sh -h`: muestra el mensaje de ayuda